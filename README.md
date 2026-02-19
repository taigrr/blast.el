# blast.el

Emacs package for [NvimBlast](https://nvimblast.com) activity tracking.

## Requirements

- Emacs 27.1+
- [blastd](https://github.com/taigrr/blastd) daemon

## Installation

### Manual

Clone the repository and add to your load path:

```elisp
(add-to-list 'load-path "/path/to/blast.el")
(require 'blast)
(blast-mode 1)
```

### use-package

```elisp
(use-package blast
  :load-path "/path/to/blast.el"
  :config
  (blast-mode 1))
```

### straight.el

```elisp
(straight-use-package
 '(blast :type git :host github :repo "taigrr/blast.el"))
(blast-mode 1)
```

### Installing blastd

Install the daemon:

```sh
go install github.com/taigrr/blastd@latest
```

Or download a binary from the [releases page](https://github.com/taigrr/blastd/releases).

## Commands

- `M-x blast-ping` - Ping the blastd daemon
- `M-x blast-status` - Show current tracking status
- `M-x blast-sync` - Trigger immediate sync to Blast server

## Configuration

All options are customizable via `M-x customize-group RET blast RET`:

```elisp
(setq blast-socket-path (expand-file-name "~/.local/share/blastd/blastd.sock"))
(setq blast-idle-timeout 120)  ; seconds before ending idle session
(setq blast-debounce-ms 1000)  ; debounce for word count updates
(setq blast-debug nil)         ; enable debug messages
(setq blast-ignored-major-modes '(dired-mode special-mode))
```

## Project Configuration

Create a `.blast.toml` anywhere in your project tree:

```toml
# Override the project name (default: git directory name)
name = "my-project"

# Mark as private — activity is still synced, but project name and git branch/remote
# are replaced with "private" so the server only sees time, filetype, and metrics
private = true
```

The file is discovered by walking up from the current buffer's directory to the nearest git root. Both fields are optional.

### Monorepos

In a monorepo, you can place `.blast.toml` in any subdirectory to give it a distinct project name or mark it as private.
The closest `.blast.toml` between the file and the git root wins:

```
monorepo/               ← git root
├── .blast.toml         ← name = "monorepo" (fallback)
├── apps/
│   ├── web/
│   │   └── .blast.toml ← name = "web"
│   └── api/
│       └── .blast.toml ← name = "api", private = true
└── packages/
    └── shared/         ← inherits "monorepo" from root .blast.toml
```

## Private mode

For global privacy (all projects), set `metrics_only = true` in your [blastd config](https://github.com/taigrr/blastd#privacy) or `BLAST_METRICS_ONLY=true`.

## How It Works

1. The package tracks buffer activity and text changes
2. Sessions are created per-project (detected via git or `.blast.toml`)
3. Activity is sent to the local blastd daemon via Unix socket
4. blastd syncs to the Blast server every 10 minutes

### Tracked Metrics

- Time spent per project
- Major mode (filetype) breakdown
- Actions per minute (edits, commands)
- Words per minute

## Statusline

Add to your mode line:

```elisp
;; Show "Blast" in mode line when connected (built-in with blast-mode lighter)
;; Or customize with:
(setq blast-mode-lighter " 🚀")  ; when connected
```

## Related Projects

- [blastd](https://github.com/taigrr/blastd) - Local daemon
- [blast.nvim](https://github.com/taigrr/blast.nvim) - Neovim plugin
