import * as vscode from "vscode";
import {
  CloseAction,
  ErrorAction,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function stopClient(): void {
  void client?.stop().catch(() => undefined);
}

export function activate(context: vscode.ExtensionContext): void {
  const command = "kaede";
  const args = ["lsp"];

  const serverOptions: ServerOptions = {
    command,
    args,
    transport: TransportKind.stdio
  };

  const outputChannel = vscode.window.createOutputChannel("Kaede LSP");
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "kaede", scheme: "file" }],
    outputChannel,
    errorHandler: {
      error: (error) => {
        outputChannel.appendLine(error.message);
        return { action: ErrorAction.Shutdown };
      },
      closed: () => ({ action: CloseAction.DoNotRestart })
    },
    initializationOptions: {
      rootUri: vscode.workspace.workspaceFolders?.[0]?.uri?.toString() ?? null
    }
  };

  client = new LanguageClient(
    "kaedeLsp",
    "Kaede Language Server",
    serverOptions,
    clientOptions
  );

  void client.start().catch(() => undefined);
  context.subscriptions.push({
    dispose: () => {
      stopClient();
    }
  });
}

export async function deactivate(): Promise<void> {
  stopClient();
  client = undefined;
}
