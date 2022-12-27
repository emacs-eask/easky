[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/easky.svg)](https://jcs-emacs.github.io/jcs-elpa/#/easky)
<a href="#"><img align="right" src="https://raw.githubusercontent.com/emacs-eask/cli/master/docs/static/logo.png" width="20%"></a>

# easky
> Control Eask in Emacs

[![CI](https://github.com/emacs-eask/easky/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-eask/easky/actions/workflows/test.yml)

`Easky` is the interface to control [Eask CLI](https://github.com/emacs-eask/cli),
it's a package manager and sandbox tools to test and manage your elisp packages.

## 🔰 Getting Started

### 🔍 Step 1. Install [Eask CLI](https://github.com/emacs-eask/cli)

Download binaries from their [release page](https://github.com/emacs-eask/cli/releases)
, and extracted somewhere on your machine. Then add the path `/path/to/eask/`
to environment `PATH`, so you can access it anywhere.

> 💡 For more installation options, see https://emacs-eask.github.io/Getting-Started/Install-Eask/.

### 🔍 Step 2. Install `Easky` (this package)

#### package.el

This package is available from [JCS-ELPA](https://jcs-emacs.github.io/jcs-elpa/).
Install from these repositories then you should be good to go!

Normally, you don't need to add `(require 'easky)` to your configuration since
most `easky` commands are autoloads and can be called without loading the module!

#### use-package

If you use [use-package](https://www.emacswiki.org/emacs/UsePackage), add the
following to your `init.el` file:

```elisp
(use-package easky :ensure t)
```

or with `straight.el`:

```elisp
(use-package easky
  :straight (easky :type git :host github :repo "emacs-eask/easky"))
```

#### Manual installation

Copy all `.el` files in this repository to `~/.emacs.d/lisp` and add the
following:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'easky)
```

## Support Commands

WIP

## 🔧 Customization

### 🧪 Variables

List of variables that interacte with `easky`'s behaviour.

- `easky-executable` - Executable to eask-cli. (Default: `nil`)
- `easky-display-function` - Function to display Easky's result. (Default: `#'lv-message`)
- `easky-focus-p` - Select window after command execution. (Default: `nil`)
- `easky-move-point-for-output` - Controls whether interpreter output moves point to the end of the output. (Default: `nil`)
- `easky-timeout-seconds` - Timeout seconds for running too long process. (Default: `30`)

## 🔌 Plugins

WIP

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
