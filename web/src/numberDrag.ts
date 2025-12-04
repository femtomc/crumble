/**
 * CodeMirror extension for draggable numbers.
 * Drag numbers up/down to increase/decrease their values.
 */

import { EditorView, Decoration, ViewPlugin } from '@codemirror/view';
import type { DecorationSet, ViewUpdate } from '@codemirror/view';
import { RangeSetBuilder } from '@codemirror/state';

// Decoration mark for numbers
const numberMark = Decoration.mark({ class: 'cm-number-draggable' });

// Find all numbers in the document and create decorations
function findNumbers(view: EditorView): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>();
  const doc = view.state.doc;

  for (let i = 1; i <= doc.lines; i++) {
    const line = doc.line(i);
    // Create a fresh regex for each line to avoid state issues
    const numberRegex = /-?\d+\.?\d*/g;
    let match;
    while ((match = numberRegex.exec(line.text)) !== null) {
      const from = line.from + match.index;
      const to = from + match[0].length;
      builder.add(from, to, numberMark);
    }
  }

  return builder.finish();
}

// State for drag operation
let dragState: {
  startY: number;
  startValue: number;
  from: number;
  to: number;
  isFloat: boolean;
  view: EditorView;
} | null = null;

// Handle mouse events
function handleMouseDown(e: MouseEvent, view: EditorView): boolean {
  // Check if we're clicking on a draggable number
  // The target might be the span itself or a child text node
  let target = e.target as HTMLElement;

  // Walk up to find the decorated span
  while (target && !target.classList?.contains('cm-number-draggable')) {
    if (target.parentElement) {
      target = target.parentElement;
    } else {
      break;
    }
  }

  if (!target?.classList?.contains('cm-number-draggable')) {
    return false;
  }

  // Get the position in the document
  const pos = view.posAtDOM(target);
  if (pos === null) return false;

  // Find the number at this position
  const doc = view.state.doc;
  const line = doc.lineAt(pos);
  const lineText = line.text;
  const posInLine = pos - line.from;

  // Find the number that contains this position
  const numberRegex = /-?\d+\.?\d*/g;
  let match;
  while ((match = numberRegex.exec(lineText)) !== null) {
    const start = match.index;
    const end = start + match[0].length;
    if (posInLine >= start && posInLine <= end) {
      const value = parseFloat(match[0]);
      const isFloat = match[0].includes('.');

      dragState = {
        startY: e.clientY,
        startValue: value,
        from: line.from + start,
        to: line.from + end,
        isFloat,
        view,
      };

      // Add drag listeners
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);

      // Prevent text selection
      e.preventDefault();
      return true;
    }
  }

  return false;
}

function handleMouseMove(e: MouseEvent) {
  if (!dragState) return;

  const deltaY = dragState.startY - e.clientY;

  // Determine step size based on whether it's a float and modifier keys
  let step = dragState.isFloat ? 0.1 : 1;
  if (e.shiftKey) step *= 10;
  if (e.altKey) step *= 0.1;

  // Calculate new value
  const delta = Math.round(deltaY / 5) * step;
  let newValue = dragState.startValue + delta;

  // Format the value
  let newText: string;
  if (dragState.isFloat) {
    // Preserve reasonable precision
    newText = newValue.toFixed(2).replace(/\.?0+$/, (m) => m.includes('.') ? '' : m);
    // Ensure at least one decimal place for floats
    if (!newText.includes('.')) {
      newText = newValue.toFixed(1);
    }
  } else {
    newText = Math.round(newValue).toString();
  }

  // Update the document
  const { view, from, to } = dragState;
  const currentText = view.state.doc.sliceString(from, to);
  if (newText !== currentText) {
    view.dispatch({
      changes: { from, to, insert: newText },
    });
    // Update the range for the next move
    dragState.to = from + newText.length;
  }
}

function handleMouseUp() {
  dragState = null;
  document.removeEventListener('mousemove', handleMouseMove);
  document.removeEventListener('mouseup', handleMouseUp);
}

// Plugin that manages decorations and handles events
export const numberDragPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;

    constructor(view: EditorView) {
      this.decorations = findNumbers(view);
    }

    update(update: ViewUpdate) {
      if (update.docChanged || update.viewportChanged) {
        this.decorations = findNumbers(update.view);
      }
    }
  },
  {
    decorations: (v) => v.decorations,
    eventHandlers: {
      mousedown: (e, view) => handleMouseDown(e, view),
    },
  }
);

// CSS for draggable numbers
export const numberDragTheme = EditorView.baseTheme({
  '.cm-number-draggable': {
    cursor: 'ns-resize',
    borderBottom: '1px dashed rgba(255, 255, 255, 0.3)',
    '&:hover': {
      backgroundColor: 'rgba(255, 255, 255, 0.1)',
      borderBottom: '1px dashed rgba(255, 255, 255, 0.6)',
    },
  },
});
