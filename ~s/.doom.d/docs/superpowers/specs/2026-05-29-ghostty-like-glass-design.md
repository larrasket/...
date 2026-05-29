# Ghostty-like Glass Design

## Goal

Make Doom Emacs glass mode in `modules/lr-macos.el` feel closer to the Ghostty screenshot: brighter, blue-gray, hazy glass instead of a black theme with lowered opacity.

## Current Behavior

The existing implementation uses the `frame-transparency` patch parameters:

- `alpha-background` controls background opacity.
- `ns-background-blur` controls the macOS CGS blur radius.
- `ns-alpha-elements` applies alpha to all relevant frame elements.

The visible result is darker than Ghostty because `modules/lr-ui.el` sets the main background faces to pure black. Lowering frame opacity lets the desktop show through, but the black face palette still dominates the composite.

## Chosen Approach

Use a small glass-specific face palette in addition to tuning alpha and blur.

When glass is enabled, apply blue-gray backgrounds to the core structural faces:

- `default`
- `fringe`
- `line-number`
- `line-number-current-line`
- `hl-line`
- `mode-line`
- `mode-line-active`
- `mode-line-inactive`
- `header-line`
- `vertical-border`
- `window-divider`
- `window-divider-first-pixel`
- `window-divider-last-pixel`

The first preset should start near:

- `salih/alpha-background`: `0.55`
- `salih/ns-background-blur`: `44`
- main glass background: around `#273454`
- modeline/header background: around `#243050`
- inactive modeline: slightly darker
- borders/dividers: soft blue-gray instead of near-black

These values are intentionally easy to tune from Emacs with `salih/set-glass`.

## V2 Revision

The first palette was still too close to the old black Doom surface. The revised target should be more aggressive:

- `salih/alpha-background`: around `0.40`
- `salih/ns-background-blur`: `50`
- main glass background: around `#50638f`
- modeline/header background: around `#42547d`
- inactive modeline: around `#2e3a59`
- default foreground: cool off-white around `#d7e2ff`

The glass palette should also cover readable foreground and interaction faces, not just structural backgrounds:

- `default`
- `font-lock-comment-face`
- `font-lock-doc-face`
- `font-lock-keyword-face`
- `font-lock-string-face`
- `font-lock-function-name-face`
- `font-lock-variable-name-face`
- `font-lock-type-face`
- `minibuffer-prompt`
- `region`
- `isearch`
- `lazy-highlight`
- `show-paren-match`
- `cursor`
- `doom-dashboard-banner`
- `doom-dashboard-menu-title`
- `doom-dashboard-menu-desc`
- completion surfaces such as `corfu-default` and `vertico-current`

The goal is to make the whole editor read as a brighter Ghostty-like material layer, not a black theme seen through transparency.

## Toggle Behavior

`salih/toggle-glass` should change both frame parameters and face backgrounds:

- Turning glass on applies the glass frame parameters and the glass palette.
- Turning glass off restores opaque frame parameters and the existing black solid palette used by `lr-ui.el`.

This keeps the toggle honest: off means a normal opaque dark Emacs, not just glass without blur.

## Theme Reload Behavior

`salih/--apply-glass` should continue to run after theme loads and frame creation. Since theme loading can overwrite face attributes, the glass palette must be re-applied in the same path as the frame parameters.

## Scope

In scope:

- Edit `modules/lr-macos.el`.
- Keep the existing `lr-ui.el` theme customization intact.
- Add small helper functions or variables only where they make the glass mode easier to understand and tune.

Out of scope:

- Creating a full Doom theme fork.
- Changing non-glass UI colors unrelated to the glass effect.
- Matching Ghostty's renderer exactly; Emacs only exposes the patched frame alpha and macOS blur controls here.

## Testing

Verification should include:

- Byte-compiling or loading `modules/lr-macos.el` enough to catch syntax errors.
- Checking that `salih/--apply-glass`, `salih/toggle-glass`, and `salih/set-glass` still evaluate cleanly.
- Manual visual verification in graphical Emacs on macOS, because the final effect depends on the native NS frame compositor and the desktop behind the window.
