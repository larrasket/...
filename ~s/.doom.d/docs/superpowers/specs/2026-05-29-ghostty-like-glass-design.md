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
