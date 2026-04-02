import React from 'react';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import CodeBlock from '@theme/CodeBlock';

const sampleProgram = `import std.http

mut app := std.http.App::new()

app.get("/", |req, res| {
    res.send_text("hello, world!")
})

app.ws("/ws/echo", |req, ws| {
    loop {
        msg := ws.receive().unwrap()
        match msg.kind {
            std.http.WebSocketMessageKind::Close => return
            _ => ws.send(msg)
        }
    }
})

app.listen(port=8080)`;

const features = [
  {
    title: 'Concise server code',
    body: 'Garbage collection and an expression-oriented style keep HTTP services small without forcing manual memory management.',
  },
  {
    title: 'Built-in concurrency',
    body: 'Spawn tasks, communicate through typed channels, and write non-blocking services with syntax that stays compact.',
  },
  {
    title: 'Rust interop',
    body: 'Import Rust crates directly when you want CPU-bound or low-level work outside Kaede GC-managed hot paths.',
  },
  {
    title: 'Rich type system',
    body: 'Use enums, pattern matching, generics, structs, methods, and closures in the same language surface.',
  },
];

const quickLinks = [
  {
    title: 'Install Kaede',
    description: 'Set up LLVM 17, the runtime, and the standard library.',
    to: '/docs/getting-started/installation',
  },
  {
    title: 'Write your first program',
    description: 'Create a project, run it locally, and learn the project layout.',
    to: '/docs/getting-started/first-program',
  },
  {
    title: 'Explore the language',
    description: 'Read the syntax, type, concurrency, and Rust interop overviews.',
    to: '/docs/language/overview',
  },
];

const examples = [
  {
    title: 'HTTP server',
    description: 'A small service showing routing, query handling, and WebSocket echo support.',
    href: 'https://github.com/itto-hiramoto/kaede/tree/main/example/http_server',
  },
  {
    title: 'Comment app',
    description: 'A fuller demo combining Kaede, frontend assets, and Rust helpers.',
    href: 'https://github.com/itto-hiramoto/kaede/tree/main/example/comment_app',
  },
  {
    title: 'Rust interop',
    description: 'A minimal project that imports a Rust crate and calls native functions from Kaede.',
    href: 'https://github.com/itto-hiramoto/kaede/tree/main/example/rust_interop',
  },
];

export default function Home() {
  return (
    <Layout
      title="Kaede"
      description="Kaede is a language for concise servers without giving up performance."
    >
      <main className="kaede-home">
        <section className="kaede-hero">
          <div className="container kaede-hero__grid">
            <div className="kaede-hero__copy">
              <p className="kaede-eyebrow">🍁 Kaede</p>
              <h1 className="kaede-hero__title">
                A language for concise servers without giving up performance.
              </h1>
              <p className="kaede-hero__subtitle">
                Write compact services with garbage collection, concurrency
                primitives, and direct Rust interop when you need it.
              </p>
              <div className="kaede-hero__actions">
                <Link
                  className="button button--primary button--lg"
                  to="/docs/getting-started/installation"
                >
                  Get Started
                </Link>
                <Link
                  className="button button--secondary button--lg"
                  href="https://github.com/itto-hiramoto/kaede"
                >
                  View on GitHub
                </Link>
              </div>
            </div>

            <div className="kaede-sample">
              <div className="kaede-sample__label">Sample</div>
              <CodeBlock className="kaede-sample__code" language="rust">
                {sampleProgram}
              </CodeBlock>
            </div>
          </div>
        </section>

        <section className="container kaede-section">
          <div className="kaede-section__heading">
            <p className="kaede-eyebrow">Why Kaede</p>
            <h2>Focused on practical server-side code</h2>
          </div>
          <div className="kaede-feature-grid">
            {features.map((feature) => (
              <article key={feature.title} className="kaede-card">
                <h3>{feature.title}</h3>
                <p>{feature.body}</p>
              </article>
            ))}
          </div>
        </section>

        <section className="container kaede-section">
          <div className="kaede-section__heading">
            <p className="kaede-eyebrow">Start Here</p>
            <h2>The shortest path from install to real code</h2>
          </div>
          <div className="kaede-link-grid">
            {quickLinks.map((item) => (
              <Link key={item.title} className="kaede-card kaede-card--link" to={item.to}>
                <h3>{item.title}</h3>
                <p>{item.description}</p>
              </Link>
            ))}
          </div>
        </section>

        <section className="container kaede-section">
          <div className="kaede-section__heading">
            <p className="kaede-eyebrow">Examples</p>
            <h2>Reference projects in the repository</h2>
          </div>
          <div className="kaede-link-grid">
            {examples.map((item) => (
              <Link key={item.title} className="kaede-card kaede-card--link" href={item.href}>
                <h3>{item.title}</h3>
                <p>{item.description}</p>
              </Link>
            ))}
          </div>
        </section>
      </main>
    </Layout>
  );
}
