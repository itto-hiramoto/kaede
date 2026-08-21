# Kaede Language Support (VS Code)

Syntax highlighting and language-server integration for [Kaede](https://github.com/itto-hiramoto/kaede).

## Layout

- `syntaxes/kaede.tmLanguage.json` — TextMate grammar. Covers only what is decidable
  lexically; anything needing name resolution is left to LSP semantic tokens, which
  the Kaede language server does not implement yet. The grammar is the only
  highlighting layer today.
- `src/extension.ts` — launches `kaede lsp` over stdio.
- `tests/` — scope assertions run by
  [`vscode-tmgrammar-test`](https://github.com/PanAeon/vscode-tmgrammar-test). Each
  non-obvious rule has a fixture whose comment names the hazard it guards against.
  Fixtures are tokenized, never compiled, and several deliberately do not compile.

## Development

```bash
npm install
npm test           # grammar scope assertions
npm run compile    # tsc
```

## Gotchas

- Do not add `|$` to the `end` of a string rule. Kaede strings may span physical
  lines, so the newline guard would break legal code. The cost is that an
  unterminated string inverts quote parity for the rest of the file — string bodies
  highlight as code and code as string — with no way to recover. `char` and
  `byte-char` do carry the guard, because a char literal holds exactly one character
  or escape and so can never span a line.
