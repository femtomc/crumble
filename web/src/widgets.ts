/**
 * CodeMirror widget system for inline pattern visualizations.
 *
 * Based on Strudel's widget approach:
 * - Widgets are block decorations placed after pattern expressions
 * - Canvas elements are pre-created and stored for lookup
 * - Patterns are queried to populate the visualizations
 */

import { StateEffect, StateField } from '@codemirror/state';
import { Decoration, EditorView, WidgetType } from '@codemirror/view';
import type { DecorationSet } from '@codemirror/view';

// Types
export interface WidgetConfig {
  type: 'pianoroll' | 'scope' | 'meter';
  to: number; // Position in document (end of pattern expression)
  options?: Record<string, unknown>;
}

// Effect to add widgets
export const addWidgets = StateEffect.define<WidgetConfig[]>({
  map: (configs, change) => configs.map(c => ({
    ...c,
    to: change.mapPos(c.to),
  })),
});

// Store for widget DOM elements
const widgetElements = new Map<string, HTMLElement>();

/**
 * Get or create a canvas widget element.
 */
export function getCanvasWidget(
  id: string,
  options: { width?: number; height?: number; pixelRatio?: number } = {}
): HTMLCanvasElement {
  const { width = 400, height = 60, pixelRatio = window.devicePixelRatio || 1 } = options;

  let canvas = widgetElements.get(id) as HTMLCanvasElement | undefined;
  if (!canvas) {
    canvas = document.createElement('canvas');
    canvas.id = id;
    widgetElements.set(id, canvas);
  }

  canvas.width = width * pixelRatio;
  canvas.height = height * pixelRatio;
  canvas.style.width = `${width}px`;
  canvas.style.height = `${height}px`;
  canvas.style.display = 'block';
  canvas.style.margin = '4px 0';
  canvas.style.borderRadius = '4px';
  canvas.style.backgroundColor = 'rgba(0, 0, 0, 0.3)';

  return canvas;
}

/**
 * Set a widget element for a given ID.
 */
export function setWidget(id: string, element: HTMLElement) {
  widgetElements.set(id, element);
}

/**
 * Get a widget element by ID.
 */
export function getWidget(id: string): HTMLElement | undefined {
  return widgetElements.get(id);
}

/**
 * Generate a unique widget ID.
 */
export function getWidgetId(config: WidgetConfig, index: number): string {
  return `widget_${config.type}_${index}`;
}

/**
 * Block widget class for CodeMirror.
 */
class BlockWidget extends WidgetType {
  private config: WidgetConfig;
  private id: string;

  constructor(config: WidgetConfig, id: string) {
    super();
    this.config = config;
    this.id = id;
  }

  eq(other: BlockWidget): boolean {
    return this.id === other.id;
  }

  toDOM(): HTMLElement {
    const existing = widgetElements.get(this.id);
    if (existing) {
      return existing;
    }

    // Create a placeholder if no element exists yet
    const placeholder = document.createElement('div');
    placeholder.className = 'cm-widget-placeholder';
    placeholder.style.height = '60px';
    placeholder.style.backgroundColor = 'rgba(100, 100, 100, 0.2)';
    placeholder.style.borderRadius = '4px';
    placeholder.style.margin = '4px 0';
    placeholder.textContent = `Loading ${this.config.type}...`;
    placeholder.style.display = 'flex';
    placeholder.style.alignItems = 'center';
    placeholder.style.justifyContent = 'center';
    placeholder.style.color = 'rgba(255, 255, 255, 0.5)';
    placeholder.style.fontSize = '12px';

    return placeholder;
  }

  ignoreEvent(): boolean {
    return true;
  }
}

/**
 * Create widget decorations from configs.
 */
function createWidgetDecorations(configs: WidgetConfig[], docLength: number): DecorationSet {
  if (configs.length === 0) {
    return Decoration.none;
  }

  const decorations = configs
    .filter(config => config.to >= 0 && config.to <= docLength)
    .sort((a, b) => a.to - b.to)
    .map((config, index) => {
      const id = getWidgetId(config, index);
      return Decoration.widget({
        widget: new BlockWidget(config, id),
        side: 1,
        block: true,
      }).range(config.to);
    });

  if (decorations.length === 0) {
    return Decoration.none;
  }

  return Decoration.set(decorations);
}

/**
 * State field for managing widgets.
 */
export const widgetField = StateField.define<DecorationSet>({
  create() {
    return Decoration.none;
  },
  update(widgets, tr) {
    widgets = widgets.map(tr.changes);

    for (const effect of tr.effects) {
      if (effect.is(addWidgets)) {
        widgets = createWidgetDecorations(effect.value, tr.newDoc.length);
      }
    }

    return widgets;
  },
  provide: f => EditorView.decorations.from(f),
});

/**
 * Update widgets in the editor.
 */
export function updateWidgets(view: EditorView, configs: WidgetConfig[]) {
  view.dispatch({ effects: addWidgets.of(configs) });
}

/**
 * Clear all widgets.
 */
export function clearWidgets(view: EditorView) {
  view.dispatch({ effects: addWidgets.of([]) });
}

/**
 * Widget extension for CodeMirror.
 */
export const widgetExtension = [widgetField];
