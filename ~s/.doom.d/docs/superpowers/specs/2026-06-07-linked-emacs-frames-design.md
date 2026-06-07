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
- It should sync only when linked frames are still in the same workspace. Once a linked frame moves to a different workspace it becomes independent, even if it later displays the same buffer; Emacs already shares buffer text and unsaved edits for a shared buffer, so no extra workspace or layout sync is wanted across workspaces.
- It should enable visible cursors in non-selected windows so the mirrored point is visible in the original frame.

## Data Flow

On `M-n`, the source frame's link group, workspace name, selected buffer, window state, cursor position, and scroll position are captured. The new frame receives the same link group and displays that state. A lightweight `post-command-hook` schedules sync from the active linked frame to eligible live frames in the same group, where eligibility means the sibling frame still shares the active frame's workspace.

When a linked frame opens a file that is already visiting an existing buffer, Emacs reuses the same buffer, so unsaved edits and point movement are already shared by Emacs in any frame displaying that buffer. The sync layer additionally mirrors workspace, window layout, selected buffer, and cursor/scroll state — but only between frames in the same workspace. A frame that has switched to a different workspace is left alone.

## Testing

Add focused ERT tests for the linked-frame helpers:

- A source frame receives a stable link group and has the Doom workspace association parameter cleared.
- Applying linked state adds the buffers to the target workspace, restores window splits, selects the buffer, and restores cursor and scroll state.
- Sync applies only to linked frames in the same workspace; a frame in a different workspace is not made identical even when it shows the same buffer.
- Cursor display is enabled for non-selected windows.
- The `M-n` keybinding points at `salih/make-linked-frame`.

Manual verification still matters for real graphical frames because batch Emacs cannot reliably create macOS GUI frames.
