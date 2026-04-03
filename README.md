# discourse.el

Browse and post to [Discourse](https://www.discourse.org/) forums from Emacs.

Unlike [nndiscourse](https://github.com/dickmao/nndiscourse) (a Gnus backend), `discourse.el` is a **standalone package** with its own buffer-based UI — no Gnus, no Ruby, no external dependencies beyond Emacs 27.1+.

## Features

- **Browse categories and topics** — tabulated list views with sorting
- **Read topic threads** — HTML posts rendered via `shr`
- **Create new topics** — compose in Markdown
- **Reply to posts** — topic-level or post-level replies
- **Like posts**
- **Search** forums
- **Notifications** viewer
- **Multiple site support** — switch between forums

## Authentication

Credentials are stored in `~/.authinfo.gpg` via `auth-source`. Two auth methods are supported:

### API Key (recommended)

If the site admin has provided you an API key, add to your `.authinfo.gpg`:

```text
machine discourse.example.com login YOUR_USERNAME password YOUR_API_KEY
```

### Session (username/password)

For sites without API keys, use your login credentials:

```text
machine discourse.example.com login YOUR_USERNAME password YOUR_PASSWORD
```

Set the auth method in your config:

```elisp
;; Default: tries API key first, falls back to session
(setq discourse-api-auth-method 'auto)

;; Force API key only
(setq discourse-api-auth-method 'api-key)

;; Force session (username/password) only
(setq discourse-api-auth-method 'session)
```

## Install

### Manual

```bash
git clone https://github.com/glenneth1/discourse.el.git ~/path/to/discourse.el
```

Add to your `init.el`:

```elisp
(add-to-list 'load-path "~/path/to/discourse.el")
(require 'discourse)

;; Optional: set a default site
(setq discourse-default-url "https://discourse.example.com")
```

### use-package (manual path)

```elisp
(use-package discourse
  :load-path "~/path/to/discourse.el"
  :custom
  (discourse-default-url "https://discourse.example.com"))
```

## Usage

### Quick start

```text
M-x discourse-connect RET https://discourse.example.com RET
```

Or, if you've set `discourse-default-url`:

```text
M-x discourse
```

### Keybindings

#### Category List

| Key   | Action                          |
|-------|---------------------------------|
| `RET` | Open category (show topics)     |
| `g`   | Refresh categories              |
| `L`   | Show latest topics (all)        |
| `s`   | Search                          |
| `n`   | Show notifications              |
| `q`   | Quit                            |

#### Topic List

| Key   | Action                          |
|-------|---------------------------------|
| `RET` | Open topic (show posts)         |
| `g`   | Refresh topics                  |
| `N`   | Next page                       |
| `P`   | Previous page                   |
| `c`   | Compose new topic               |
| `s`   | Search                          |
| `q`   | Quit (back to categories)       |

#### Topic/Post View

| Key   | Action                          |
|-------|---------------------------------|
| `n`   | Next post                       |
| `p`   | Previous post                   |
| `r`   | Reply to topic                  |
| `R`   | Reply to post at point          |
| `l`   | Like post at point              |
| `b`   | Open in browser                 |
| `g`   | Refresh                         |
| `SPC` | Scroll down                     |
| `DEL` | Scroll up                       |
| `q`   | Quit                            |

#### Compose

| Key       | Action   |
|-----------|----------|
| `C-c C-c` | Send     |
| `C-c C-k` | Cancel   |
| `C-c C-p` | Preview  |

## Differences from nndiscourse

| Feature          | nndiscourse                    | discourse.el              |
|------------------|--------------------------------|---------------------------|
| **UI**           | Gnus (newsreader)              | Standalone buffers        |
| **Dependencies** | Ruby, rbenv, bundler, Cask     | None (pure Elisp)         |
| **Auth**         | Public sites only (originally) | API key + session login   |
| **Posting**      | Not implemented                | Full support              |
| **Emacs version**| 27.1+                          | 27.1+                     |

## License

GPLv3. See [LICENSE](LICENSE).
