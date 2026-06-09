# Linked Emacs Frames Design

## Goal

`M-n` should open a new Emacs frame that stays linked to the frame it came from. The linked frames should use the same Doom workspace and keep the visible editing state in sync: selected buffer, window splits, cursor position, and scroll position. Buffer text and unsaved edits are already shared by Emacs when the same buffer is displayed, so the config should make linked frames display the same buffer rather than creating duplicate file buffers.

## Current Behavior

Doom's `:ui workspaces` module uses `persp-mode`. Interactive `make-frame` normally runs Doom's frame association function, which creates a fresh workspace for the new frame. The current config binds `M-n` directly to `make-frame`, so it inherits that behavior.

## Proposed Behavior

Replace the `M-n` binding with `salih/make-linked-frame`. This command creates a frame, tags the source and target frames with a shared link group, switches the target frame to the source workspace, mirrors the selected buffer and cursor state, and keeps future commands in either linked frame synchronized.

The sync layer should be narrow and reversible:

- It should affect frames created through `salih/make-linked-frame`.
- It should not change Doom's behavior for other frame creation paths.
- It should clear Doom's `workspace` frame parameter on linked frames so deleting one linked frame does not delete the shared workspace.
- It should use Doom/persp APIs when available, with graceful fallback when tests load the module outside Doom.
- It should mirror in two modes. When linked frames share a workspace, it does a full sync: workspace, window layout, selected buffer, cursor, and scroll. When a linked frame has moved to a different workspace, it does a light sync limited to any buffer the two frames already share: only that buffer's cursor and scroll position are mirrored, and never the workspace or window layout. A frame in a different workspace that does not display the source buffer is left alone.
- It should enable visible cursors in non-selected windows so the mirrored point is visible in the original frame.

## Data Flow

On `M-n`, the source frame's link group, workspace name, selected buffer, window state, cursor position, and scroll position are captured. The new frame receives the same link group and displays that state. A `post-command-hook` synchronously mirrors the active linked frame to its sibling frames in the same group. For each sibling the sync mode is chosen by `salih/--linked-frame-sync-mode`: `full` when the sibling shares the active frame's workspace, `buffer` when it is in a different workspace but already displays the active buffer, and nil (skip) otherwise.

When a linked frame opens a file that is already visiting an existing buffer, Emacs reuses the same buffer, so unsaved edits are shared automatically in any frame displaying it. On top of that, the full-sync mode mirrors workspace, window layout, selected buffer, and cursor/scroll for same-workspace siblings, while the light buffer-sync mode mirrors only the shared buffer's cursor/scroll into whatever window of a different-workspace sibling already shows it — without touching that sibling's workspace or layout.

## Testing

Add focused ERT tests for the linked-frame helpers:

- A source frame receives a stable link group and has the Doom workspace association parameter cleared.
- Applying linked state adds the buffers to the target workspace, restores window splits, selects the buffer, and restores cursor and scroll state.
- Sync mode is `full` for same-workspace siblings, `buffer` for different-workspace siblings that show the source buffer, and nil otherwise.
- The light buffer-sync mirrors the shared buffer's scroll/cursor without switching workspace or rebuilding the window layout.
- Cursor display is enabled for non-selected windows.
- The `M-n` keybinding points at `salih/make-linked-frame`.

Manual verification still matters for real graphical frames because batch Emacs cannot reliably create macOS GUI frames.
