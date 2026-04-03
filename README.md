# discourse.el

Browse and post to [Discourse](https://www.discourse.org/) forums from Emacs.

Unlike [nndiscourse](https://github.com/dickmao/nndiscourse) (a Gnus backend), `discourse.el` is a **standalone package** with its own buffer-based UI — no Gnus, no Ruby, no external dependencies beyond Emacs 27.1+.

## Features

- **Persistent sidebar** — categories, navigation items, unread/new counts
- **Split-window layout** — sidebar stays visible alongside topics and posts
- **Browse categories and topics** — with pagination and unread highlighting
- **Read topic threads** — HTML posts rendered via `shr`
- **Create new topics** — compose in Markdown
- **Reply to posts** — topic-level or post-level replies
- **Like posts**
- **Search** forums
- **Notifications** viewer
- **Multiple site support** — switch between forums with per-site config

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

## Install

### Manual

```bash
git clone https://github.com/glenneth1/discourse.el.git ~/path/to/discourse.el
```

Add to your `init.el`:

```elisp
(add-to-list 'load-path "~/path/to/discourse.el")
(require 'discourse)
(setq discourse-default-url "https://discourse.example.com")
```

### use-package

```elisp
(use-package discourse
  :load-path "~/path/to/discourse.el"
  :commands (discourse discourse-connect discourse-switch-site)
  :custom
  (discourse-default-url "https://discourse.example.com")
  (discourse-api-auth-method 'auto)
  (discourse-site-configs
   '(("https://discourse.example.com" :auth-method session)
     ("https://meta.discourse.org" :auth-method api-key))))
```

## Configuration

### Auth method

```elisp
;; 'auto (default) — tries API key first, falls back to session
;; 'api-key       — API key only
;; 'session       — username/password only
(setq discourse-api-auth-method 'auto)
```

### Per-site config

Override the auth method per site:

```elisp
(setq discourse-site-configs
      '(("https://forum.example.com" :auth-method session)
        ("https://meta.discourse.org" :auth-method api-key)))
```

### Sidebar width

```elisp
(setq discourse-ui-sidebar-width 35)  ; default: 35 columns
```

### Topic page size

```elisp
(setq discourse-ui-topic-page-size 30)  ; default: 30 topics per page
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

To switch between multiple forums:

```text
M-x discourse-switch-site
```

Use `C-u M-x discourse-connect` to force a URL prompt even when a default is set.

### Keybindings

#### Sidebar (Categories)

| Key       | Action                          |
|-----------|---------------------------------|
| `RET`     | Open item (category/nav)        |
| `n` / `p` | Next / previous item            |
| `TAB`     | Next item                       |
| `o`       | Switch to content window        |
| `s`       | Search                          |
| `g`       | Refresh                         |
| `q`       | Quit                            |

#### Topic List

| Key   | Action                          |
|-------|---------------------------------|
| `RET` | Open topic (show posts)         |
| `n`   | Next topic                      |
| `p`   | Previous topic                  |
| `N`   | Next page                       |
| `P`   | Previous page                   |
| `c`   | Compose new topic               |
| `o`   | Switch to sidebar               |
| `s`   | Search                          |
| `g`   | Refresh                         |
| `q`   | Quit (back to sidebar)          |

#### Topic/Post View

| Key   | Action                          |
|-------|---------------------------------|
| `n`   | Next post                       |
| `p`   | Previous post                   |
| `r`   | Reply to topic                  |
| `R`   | Reply to post at point          |
| `l`   | Like post at point              |
| `o`   | Switch to sidebar               |
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
| **UI**           | Gnus (newsreader)              | Standalone buffers + sidebar |
| **Dependencies** | Ruby, rbenv, bundler, Cask     | None (pure Elisp)         |
| **Auth**         | Public sites only (originally) | API key + session login   |
| **Posting**      | Not implemented                | Full support              |
| **Emacs version**| 27.1+                          | 27.1+                     |

## License

GPLv3. See [LICENSE](LICENSE).
