/**
 * CodeMirror extension for highlighting active pattern events.
 *
 * Uses Strudel's two-phase approach:
 * 1. At eval time: create marks for ALL source locations (invisible by default)
 * 2. Per frame: show marks that match currently active haps
 *
 * This allows CodeMirror to automatically remap marks when the document is edited.
 */

import { EditorView, Decoration } from '@codemirror/view';
import type { DecorationSet } from '@codemirror/view';
import { RangeSetBuilder, StateField, StateEffect, Prec } from '@codemirror/state';

// Types
export interface SourceLocation {
  start: number;
  end: number;
}

export interface ActiveHap {
  whole_start?: number;
  whole_end?: number;
  locations?: SourceLocation[];
  value?: unknown;
}

// Effect to set ALL locations at eval time
export const setAllLocations = StateEffect.define<SourceLocation[]>();

// Effect to show active locations per frame
export const showActiveLocations = StateEffect.define<{ time: number; haps: ActiveHap[] }>();

/**
 * Update all source locations (call at eval time).
 * This creates invisible marks that will be shown when events are active.
 */
export function updateAllLocations(view: EditorView, locations: SourceLocation[]) {
  view.dispatch({ effects: setAllLocations.of(locations) });
}

/**
 * Highlight currently active events (call per frame).
 */
export function highlightActiveHaps(view: EditorView, time: number, haps: ActiveHap[]) {
  view.dispatch({ effects: showActiveLocations.of({ time, haps }) });
}

/**
 * State field that stores all source locations as invisible marks.
 * These marks are created at eval time and move with document edits.
 */
const allLocationsField = StateField.define<DecorationSet>({
  create() {
    return Decoration.none;
  },
  update(decorations, tr) {
    // Remap decorations when document changes
    if (tr.docChanged) {
      decorations = decorations.map(tr.changes);
    }

    for (const effect of tr.effects) {
      if (effect.is(setAllLocations)) {
        // Create a mark for each location, storing the range in the spec for lookup
        const marks = effect.value
          .filter(loc => loc.start < tr.newDoc.length)
          .map(loc => {
            const start = loc.start;
            const end = Math.min(loc.end, tr.newDoc.length);
            const id = `${start}:${end}`;
            return Decoration.mark({
              // Store the original range as ID for matching with active haps
              attributes: { 'data-loc-id': id },
              class: 'cm-pattern-location',
            }).range(start, end);
          });

        decorations = Decoration.set(marks, true);
      }
    }

    return decorations;
  },
});

/**
 * State field that tracks which locations are currently active.
 */
const activeLocationsField = StateField.define<{ time: number; activeIds: Set<string> }>({
  create() {
    return { time: 0, activeIds: new Set() };
  },
  update(state, tr) {
    for (const effect of tr.effects) {
      if (effect.is(showActiveLocations)) {
        const { time, haps } = effect.value;
        const activeIds = new Set<string>();

        for (const hap of haps) {
          if (!hap.locations) continue;

          for (const loc of hap.locations) {
            const id = `${loc.start}:${loc.end}`;
            activeIds.add(id);
          }
        }

        return { time, activeIds };
      }
    }
    return state;
  },
});

/**
 * Compute visible decorations by combining all locations with active state.
 */
const activeHighlights = EditorView.decorations.compute(
  [allLocationsField, activeLocationsField],
  (state) => {
    const allLocs = state.field(allLocationsField);
    const { activeIds } = state.field(activeLocationsField);

    if (activeIds.size === 0) {
      return Decoration.none;
    }

    const builder = new RangeSetBuilder<Decoration>();
    const iterator = allLocs.iter();

    while (iterator.value) {
      const { from, to, value } = iterator;
      const locId = value.spec?.attributes?.['data-loc-id'];

      if (locId && activeIds.has(locId)) {
        builder.add(
          from,
          to,
          Decoration.mark({
            class: 'cm-active-event',
          })
        );
      }

      iterator.next();
    }

    return builder.finish();
  }
);

/**
 * CSS theme for highlighting.
 */
export const eventHighlightTheme = EditorView.baseTheme({
  // Invisible by default - these are just position markers
  '.cm-pattern-location': {
    // Uncomment to debug: see all registered locations
    // backgroundColor: 'rgba(0, 255, 0, 0.1)',
  },
  // Active events get highlighted
  '.cm-active-event': {
    backgroundColor: 'rgba(255, 200, 0, 0.4)',
    borderRadius: '2px',
    boxShadow: '0 0 4px rgba(255, 200, 0, 0.6)',
  },
});

/**
 * Complete extension for event highlighting.
 * Use Prec.highest to ensure our decorations take priority.
 */
export const highlightExtension = Prec.highest([
  allLocationsField,
  activeLocationsField,
  activeHighlights,
  eventHighlightTheme,
]);

// Legacy exports for compatibility
export const activeLocationsField_legacy = activeLocationsField;
export const eventHighlightPlugin = allLocationsField; // Not really a plugin anymore, but keeps imports working

/**
 * Clear all highlights (e.g., when stopping playback).
 */
export function clearHighlights(view: EditorView) {
  view.dispatch({
    effects: showActiveLocations.of({ time: 0, haps: [] }),
  });
}

// Legacy function - maps to new API
export function highlightEvents(view: EditorView, locations: SourceLocation[]) {
  // Convert flat locations to haps format
  const haps: ActiveHap[] = locations.map(loc => ({ locations: [loc] }));
  highlightActiveHaps(view, 0, haps);
}
