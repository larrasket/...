# Linked Emacs Frames Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `M-n` open a Doom workspace-linked frame that mirrors selected buffer, window splits, cursor position, and scroll position with its source frame.

**Architecture:** Add a small sync layer to `modules/lr-editor.el`. Frames created by `salih/make-linked-frame` share a frame-parameter group; a post-command idle sync propagates the active linked frame's window state and selected-window state to sibling linked frames only when they are in the same workspace or already showing the same buffer.

**Tech Stack:** Doom Emacs Lisp, `persp-mode`/Doom workspaces, ERT batch tests.

---

### Task 1: Tests For Linked-Frame Helpers

**Files:**
- Create: `test/lr-editor-linked-frame-test.el`

- [ ] **Step 1: Write the failing tests**

Create ERT tests that load `modules/lr-editor.el` with Doom macros stubbed, then verify group setup, state application, window layout capture/restore, same-workspace/same-buffer sync gating, non-selected cursor display, and the `M-n` binding target.

- [ ] **Step 2: Run the test to verify it fails**

Run: `emacs -Q --batch -L modules -l test/lr-editor-linked-frame-test.el -f ert-run-tests-batch-and-exit`

Expected: fail because `salih/make-linked-frame` and linked-frame helpers do not exist yet.

### Task 2: Linked-Frame Implementation

**Files:**
- Modify: `modules/lr-editor.el`
- Test: `test/lr-editor-linked-frame-test.el`

- [ ] **Step 1: Add linked-frame state helpers**

Add helpers for creating a link group, reading frame state, adding buffers to the current perspective, switching target workspaces, restoring window state, and applying selected-buffer/point/scroll state.

- [ ] **Step 2: Add sync scheduling**

Add a guarded `post-command-hook` function that schedules a short idle sync from the active linked frame to sibling frames in the same link group when they share the source workspace or source buffer.

- [ ] **Step 3: Add `salih/make-linked-frame`**

Create a frame, tag source and target frames with the same link group, clear Doom's `workspace` frame parameter on linked frames, apply the source state, focus the new frame, and return it.

- [ ] **Step 4: Rebind `M-n`**

Change the existing `M-n` binding from `make-frame` to `salih/make-linked-frame`.

- [ ] **Step 5: Run verification**

Run:

```bash
emacs -Q --batch -L modules -l test/lr-editor-linked-frame-test.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L modules -f batch-byte-compile modules/lr-editor.el
```

Expected: linked-frame tests pass and `lr-editor.el` byte-compiles without syntax errors.
