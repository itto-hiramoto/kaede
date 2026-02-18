use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use kaede_parse::{ParseError, Parser};
use kaede_semantic::{SemanticAnalyzer, SemanticError};
use kaede_span::{file::FilePath, Location, Span};
use kaede_type_infer::TypeInferError;
use tokio::sync::RwLock;
use tower_lsp::{
    async_trait,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, InitializeParams, InitializeResult, MessageType, Position,
        Range, ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind,
        Url,
    },
    Client, LanguageServer, LspService, Server,
};

#[derive(Debug)]
pub struct Backend {
    client: Client,
    documents: RwLock<HashMap<Url, String>>,
    root_dir: RwLock<Option<PathBuf>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: RwLock::new(HashMap::new()),
            root_dir: RwLock::new(None),
        }
    }

    async fn analyze_and_publish(&self, uri: Url, text: String) {
        let file_path = match uri.to_file_path() {
            Ok(path) => path,
            Err(_) => {
                self.client
                    .log_message(MessageType::ERROR, "Invalid file URI")
                    .await;
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
                return;
            }
        };

        let root_dir = self
            .root_dir
            .read()
            .await
            .clone()
            .unwrap_or_else(|| fallback_root_dir(&file_path));

        let diagnostics = tokio::task::spawn_blocking(move || {
            let file = FilePath::from(file_path.clone());
            let ast = match Parser::new(&text, file).run() {
                Ok(ast) => ast,
                Err(err) => return diagnostics_from_parse_error(err),
            };

            let mut analyzer = SemanticAnalyzer::new(file, root_dir);
            match analyzer.analyze(ast, false, false) {
                Ok(_) => Vec::new(),
                Err(err) => diagnostics_from_semantic_error(err),
            }
        })
        .await
        .unwrap_or_else(|err| vec![diagnostic_with_span(None, err.to_string())]);

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

pub async fn run_stdio() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        params: InitializeParams,
    ) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        if let Some(root_uri) = params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                *self.root_dir.write().await = Some(path);
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "kaede-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Kaede LSP initialized")
            .await;
    }

    async fn did_open(&self, params: tower_lsp::lsp_types::DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.documents
            .write()
            .await
            .insert(uri.clone(), text.clone());
        self.analyze_and_publish(uri, text).await;
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params
            .content_changes
            .into_iter()
            .last()
            .map(|change| change.text)
            .unwrap_or_default();

        self.client
            .publish_diagnostics(uri.clone(), Vec::new(), None)
            .await;
        self.documents
            .write()
            .await
            .insert(uri.clone(), text.clone());
        self.analyze_and_publish(uri, text).await;
    }

    async fn did_close(&self, params: tower_lsp::lsp_types::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.write().await.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }
}

fn fallback_root_dir(file_path: &Path) -> PathBuf {
    file_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

fn location_to_position(location: Location) -> Position {
    let line = location.line.saturating_sub(1).max(0) as u32;
    let character = location.column.saturating_sub(1).max(0) as u32;
    Position::new(line, character)
}

fn span_to_range(span: Span) -> Range {
    Range::new(
        location_to_position(span.start),
        location_to_position(span.finish),
    )
}

pub fn file_path_to_url(file_path: FilePath) -> Option<Url> {
    Url::from_file_path(file_path.path()).ok()
}

fn diagnostic_with_span(span: Option<Span>, message: String) -> Diagnostic {
    let range = span
        .map(span_to_range)
        .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        ..Diagnostic::default()
    }
}

fn diagnostics_from_parse_error(err: anyhow::Error) -> Vec<Diagnostic> {
    match err.downcast::<ParseError>() {
        Ok(parse_err) => vec![diagnostic_with_span(
            parse_error_span(&parse_err),
            parse_err.to_string(),
        )],
        Err(err) => vec![diagnostic_with_span(None, err.to_string())],
    }
}

fn diagnostics_from_semantic_error(err: anyhow::Error) -> Vec<Diagnostic> {
    match err.downcast::<SemanticError>() {
        Ok(semantic_err) => vec![diagnostic_with_span(
            semantic_error_span(&semantic_err),
            semantic_err.to_string(),
        )],
        Err(err) => match err.downcast::<TypeInferError>() {
            Ok(type_err) => vec![diagnostic_with_span(
                type_infer_error_span(&type_err),
                type_err.to_string(),
            )],
            Err(err) => vec![diagnostic_with_span(None, err.to_string())],
        },
    }
}

fn parse_error_span(err: &ParseError) -> Option<Span> {
    match err {
        ParseError::ExpectedError { span, .. }
        | ParseError::OutOfRangeForI32(span)
        | ParseError::OutOfRangeForU32(span) => Some(*span),
    }
}

fn semantic_error_span(err: &SemanticError) -> Option<Span> {
    match err {
        SemanticError::Undeclared { span, .. }
        | SemanticError::NotCallable { span, .. }
        | SemanticError::InvalidTypeForArithmeticOperation { span, .. }
        | SemanticError::GenericArgumentLengthMismatch { span, .. }
        | SemanticError::CannotCallMutableMethodOnImmutableValue { span }
        | SemanticError::ExpectedVariable { span, .. }
        | SemanticError::CatchAllArmMustBeLast { span }
        | SemanticError::MatchCannotBeUsedWithValueOfType { span, .. }
        | SemanticError::TooFewArguments { span, .. }
        | SemanticError::TooManyArguments { span, .. }
        | SemanticError::UnknownParameterName { span, .. }
        | SemanticError::DuplicateArgument { span, .. }
        | SemanticError::PositionalArgumentAfterKeyword { span }
        | SemanticError::AlreadyDeclared { span, .. }
        | SemanticError::VoidVariable { span, .. }
        | SemanticError::BreakOutsideOfLoop { span }
        | SemanticError::MismatchedTypes { span, .. }
        | SemanticError::InvalidLeftOfAssignment { span }
        | SemanticError::CannotAssignTwiceToImutable { span }
        | SemanticError::HasNoFields { span }
        | SemanticError::NoField { span, .. }
        | SemanticError::FileNotFoundForModule { span, .. }
        | SemanticError::TupleRequireAccessByIndex { span }
        | SemanticError::IndexOutOfRange { span, .. }
        | SemanticError::ArrayRepeatCountNotConst { span }
        | SemanticError::ArrayRepeatCountTooLarge { span, .. }
        | SemanticError::NumberOfTupleFieldsDoesNotMatch { span, .. }
        | SemanticError::IfMustHaveElseUsedAsExpr { span }
        | SemanticError::IfAndElseHaveIncompatibleTypes { span, .. }
        | SemanticError::CannotAssignImmutableToMutable { span }
        | SemanticError::NoMember { span, .. }
        | SemanticError::NoMethod { span, .. }
        | SemanticError::NoVariant { span, .. }
        | SemanticError::NotAnEnum { span, .. }
        | SemanticError::CannotAssignValueToVariant { span, .. }
        | SemanticError::NotIndexable { span, .. }
        | SemanticError::UnreachablePattern { span }
        | SemanticError::NeverIfExpr { span }
        | SemanticError::MatchNotExhaustive { span, .. }
        | SemanticError::MatchMustHaveNonCatchAllArm { span }
        | SemanticError::DuplicatePattern { span, .. }
        | SemanticError::UnitVariantCannotUnpack { span, .. }
        | SemanticError::ExpectedTypeInCast { span }
        | SemanticError::SpawnTargetNotCall { span }
        | SemanticError::SpawnReturnTypeNotUnit { span, .. }
        | SemanticError::UnsupportedLanguageLinkage { span, .. }
        | SemanticError::FormatTemplateMustBeStringLiteral { span }
        | SemanticError::FormatArgumentMustBeStr { span, .. }
        | SemanticError::InvalidFormatTemplate { span, .. }
        | SemanticError::FormatPlaceholderCountMismatch { span, .. } => Some(*span),
        SemanticError::MainNotFound
        | SemanticError::LLVMError { .. }
        | SemanticError::FailedToLookupTarget { .. }
        | SemanticError::FailedToCreateTargetMachine => None,
    }
}

fn type_infer_error_span(err: &TypeInferError) -> Option<Span> {
    match err {
        TypeInferError::ArraySizeMismatch { span, .. }
        | TypeInferError::ExpectedArrayType { span, .. }
        | TypeInferError::TupleArityMismatch { span, .. }
        | TypeInferError::ExpectedTupleType { span, .. }
        | TypeInferError::UndefinedVariable { span, .. }
        | TypeInferError::FieldNotFound { span, .. }
        | TypeInferError::NotATuple { span, .. }
        | TypeInferError::TupleIndexOutOfBounds { span, .. }
        | TypeInferError::StrIndexOutOfBounds { span }
        | TypeInferError::NotIndexable { span }
        | TypeInferError::ArgumentCountMismatch { span, .. }
        | TypeInferError::TupleUnpackCountMismatch { span, .. }
        | TypeInferError::ExpectedTupleForUnpack { span }
        | TypeInferError::CannotInferType { span }
        | TypeInferError::CannotInferVariableType { span }
        | TypeInferError::InvalidIntegerLiteralType { span, .. }
        | TypeInferError::CannotUnify { span, .. } => Some(*span),
        TypeInferError::OccursCheckFailed { .. }
        | TypeInferError::TupleArityMismatchInUnify { .. } => None,
    }
}
