/**
 * CodeMirror extension for highlighting active pattern events.
 * Events are highlighted based on their source locations as they play.
 */

import { EditorView, Decoration, ViewPlugin } from '@codemirror/view';
import type { DecorationSet } from '@codemirror/view';
import { RangeSetBuilder, StateField, StateEffect } from '@codemirror/state';

// Effect to update active locations
export const setActiveLocations = StateEffect.define<Array<{ start: number; end: number }>>();

// Decoration for active events
const activeEventMark = Decoration.mark({ class: 'cm-active-event' });

// State field to track active locations
export const activeLocationsField = StateField.define<Array<{ start: number; end: number }>>({
  create() {
    return [];
  },
  update(value, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setActiveLocations)) {
        return effect.value;
      }
    }
    return value;
  },
});

// Plugin that creates decorations based on active locations
export const eventHighlightPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;

    constructor(view: EditorView) {
      this.decorations = this.buildDecorations(view);
    }

    update(update: { view: EditorView; docChanged: boolean; state: { field: (f: typeof activeLocationsField) => Array<{ start: number; end: number }> } }) {
      // Rebuild decorations when locations change
      const locations = update.state.field(activeLocationsField);
      const oldLocations = update.view.state.field(activeLocationsField);
      if (update.docChanged || locations !== oldLocations) {
        this.decorations = this.buildDecorations(update.view);
      }
    }

    buildDecorations(view: EditorView): DecorationSet {
      const locations = view.state.field(activeLocationsField);
      if (locations.length === 0) {
        return Decoration.none;
      }

      const builder = new RangeSetBuilder<Decoration>();
      const docLength = view.state.doc.length;

      // Sort locations by start position for proper builder ordering
      const sortedLocations = [...locations].sort((a, b) => a.start - b.start);

      for (const loc of sortedLocations) {
        // Clamp to document bounds
        const start = Math.max(0, Math.min(loc.start, docLength));
        const end = Math.max(start, Math.min(loc.end, docLength));
        if (start < end) {
          builder.add(start, end, activeEventMark);
        }
      }

      return builder.finish();
    }
  },
  {
    decorations: (v) => v.decorations,
  }
);

// CSS for active event highlighting
export const eventHighlightTheme = EditorView.baseTheme({
  '.cm-active-event': {
    backgroundColor: 'rgba(255, 200, 0, 0.3)',
    borderRadius: '2px',
    transition: 'background-color 0.1s ease-out',
  },
});

// Helper to update active events in the editor
export function highlightEvents(
  view: EditorView,
  locations: Array<{ start: number; end: number }>
) {
  view.dispatch({
    effects: setActiveLocations.of(locations),
  });
}

// Helper to clear all highlights
export function clearHighlights(view: EditorView) {
  view.dispatch({
    effects: setActiveLocations.of([]),
  });
}
