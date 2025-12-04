import { EditorView, basicSetup } from 'codemirror';
import { keymap } from '@codemirror/view';
import type { ViewUpdate } from '@codemirror/view';
import { HighlightStyle, syntaxHighlighting, StreamLanguage } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import init, { evalLisp, getWidgets, JsPattern } from 'crumble';
import { AudioEngine, PatternScheduler } from './audio';
import type { SoundEvent } from './audio';
import { OfflineRenderer, encodeWAV, downloadBlob, formatDuration } from './export';
import { numberDragPlugin, numberDragTheme } from './numberDrag';
import {
  highlightExtension,
  updateAllLocations,
  highlightActiveHaps,
  clearHighlights,
  type SourceLocation as HighlightLocation,
  type ActiveHap,
} from './eventHighlight';
import { drawPianoroll, type PianorollHap } from './pianoroll';
import { drawScope, type ScopeHap } from './scope';
import {
  widgetExtension,
  updateWidgets,
  getCanvasWidget,
  type WidgetConfig,
} from './widgets';
import './style.css';

// Widget type from WASM
interface WasmWidgetConfig {
  type: 'pianoroll' | 'scope' | 'meter';
  id: string;
  start: number;
  end: number;
}

// Debounce helper
function debounce<T extends (...args: unknown[]) => void>(fn: T, delay: number): T {
  let timeoutId: ReturnType<typeof setTimeout>;
  return ((...args: unknown[]) => {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => fn(...args), delay);
  }) as T;
}

// Monospace theme with actual color values (CSS vars don't work in CM themes)
const monospaceTheme = EditorView.theme({
  '&': {
    height: '100%',
    backgroundColor: '#000',
    color: '#fff',
    fontSize: '16px',
  },
  '.cm-scroller': {
    overflow: 'auto',
    fontFamily: '"Berkeley Mono", "JetBrains Mono", "SF Mono", Consolas, monospace',
    lineHeight: '1.5',
  },
  '.cm-content': {
    caretColor: '#fff',
    fontFamily: '"Berkeley Mono", "JetBrains Mono", "SF Mono", Consolas, monospace',
    padding: '1rem',
  },
  '.cm-cursor, .cm-dropCursor': {
    borderLeftColor: '#fff',
    borderLeftWidth: '2px',
  },
  '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection': {
    backgroundColor: 'rgba(255, 255, 255, 0.2)',
  },
  '.cm-activeLine': {
    backgroundColor: '#111',
  },
  '.cm-gutters': {
    backgroundColor: '#000',
    color: '#666',
    border: 'none',
    borderRight: '2px solid #fff',
  },
  '.cm-activeLineGutter': {
    backgroundColor: '#111',
  },
  '.cm-lineNumbers .cm-gutterElement': {
    padding: '0 1ch 0 2ch',
    minWidth: '4ch',
  },
  '.cm-foldGutter': {
    width: '0',
  },
  '.cm-line': {
    padding: '0',
    lineHeight: '1.5rem',
  },
  '&.cm-focused': {
    outline: 'none',
  },
  '.cm-matchingBracket': {
    backgroundColor: 'rgba(255, 255, 255, 0.3)',
    outline: '1px solid #fff',
  },
}, { dark: true });

// Minimal syntax highlighting for Lisp
const monospaceHighlight = HighlightStyle.define([
  { tag: tags.comment, color: '#666', fontStyle: 'italic' },
  { tag: tags.keyword, color: '#fff', fontWeight: 'bold' },
  { tag: tags.atom, color: '#fff' },
  { tag: tags.number, color: '#fff' },
  { tag: tags.string, color: '#888' },
  { tag: tags.variableName, color: '#fff' },
  { tag: tags.function(tags.variableName), color: '#fff', fontWeight: 'bold' },
  { tag: tags.paren, color: '#666' },
  { tag: tags.bracket, color: '#666' },
  { tag: tags.meta, color: '#888' },
]);

// Global state
let audio: AudioEngine;
let scheduler: PatternScheduler;
let currentPattern: JsPattern | null = null;
let editor: EditorView;

// Active highlights tracking - stores haps with their scheduled times
const activeHaps = new Map<number, { hap: ActiveHap; endTime: number }>();
let hapIdCounter = 0;
let highlightFrameId: number | null = null;

// Pianoroll state
let pianorollCanvas: HTMLCanvasElement | null = null;
let pianorollCtx: CanvasRenderingContext2D | null = null;
let pianorollFrameId: number | null = null;
let pianorollHaps: PianorollHap[] = [];

// Inline widget state
interface InlineWidget {
  id: string;
  type: string;
  canvas: HTMLCanvasElement;
  ctx: CanvasRenderingContext2D;
  frameId: number | null;
}
const inlineWidgets = new Map<string, InlineWidget>();

// Initialize WASM and audio
async function initApp() {
  // Initialize WASM module
  await init();
  console.log('crumble WASM initialized');

  // Create audio engine (will be started on user interaction)
  audio = new AudioEngine({ gain: 0.3 });
  scheduler = new PatternScheduler(audio);
  scheduler.cps = 0.5; // Default tempo

  // Set up event callback to collect haps for highlighting
  scheduler.onEvent = (event, triggerTime, duration) => {
    if (!editor || !event.locations || event.locations.length === 0) return;

    // Store this hap with its end time
    const hapId = hapIdCounter++;
    const endTime = triggerTime + Math.min(duration, 0.2); // Max 200ms highlight

    activeHaps.set(hapId, {
      hap: {
        whole_start: event.whole_start,
        whole_end: event.whole_end,
        locations: event.locations,
        value: event.value,
      },
      endTime,
    });

    // Schedule removal
    const now = audio.currentTime;
    const removeDelay = Math.max(0, (endTime - now) * 1000);
    setTimeout(() => {
      activeHaps.delete(hapId);
    }, removeDelay);
  };

  // Set up UI
  setupUI();
  setupEditor();
  setupPianoroll();
  setupControls();

  // Update status
  updateStatus('Ready. Press Ctrl+Enter to evaluate.');
}

// Start the highlight animation frame loop
function startHighlightLoop() {
  if (highlightFrameId !== null) return;

  const updateFrame = () => {
    if (!editor) return;

    const now = audio.currentTime;

    // Filter to only currently active haps
    const currentHaps: ActiveHap[] = [];
    for (const { hap, endTime } of activeHaps.values()) {
      if (now < endTime) {
        currentHaps.push(hap);
      }
    }

    // Update highlights
    highlightActiveHaps(editor, now, currentHaps);

    highlightFrameId = requestAnimationFrame(updateFrame);
  };

  highlightFrameId = requestAnimationFrame(updateFrame);
}

// Stop the highlight animation frame loop
function stopHighlightLoop() {
  if (highlightFrameId !== null) {
    cancelAnimationFrame(highlightFrameId);
    highlightFrameId = null;
  }
  activeHaps.clear();
  if (editor) {
    clearHighlights(editor);
  }
}

// Set up the pianoroll canvas
function setupPianoroll() {
  pianorollCanvas = document.getElementById('pianoroll') as HTMLCanvasElement;
  if (!pianorollCanvas) return;

  const container = pianorollCanvas.parentElement!;
  const pixelRatio = window.devicePixelRatio || 1;

  // Size the canvas to fit container
  const resize = () => {
    if (!pianorollCanvas) return;
    const width = container.clientWidth;
    const height = 80;
    pianorollCanvas.width = width * pixelRatio;
    pianorollCanvas.height = height * pixelRatio;
    pianorollCanvas.style.width = `${width}px`;
    pianorollCanvas.style.height = `${height}px`;
  };

  resize();
  window.addEventListener('resize', resize);

  pianorollCtx = pianorollCanvas.getContext('2d');
}

// Start the pianoroll animation loop
function startPianorollLoop() {
  if (pianorollFrameId !== null || !pianorollCtx || !currentPattern) return;

  const cycles = 4;
  const playhead = 0.5;

  const animate = () => {
    if (!pianorollCtx || !currentPattern || !scheduler.isRunning) {
      pianorollFrameId = null;
      return;
    }

    const time = audio.currentTime;

    // Query haps for visible time range
    const from = time - cycles * playhead;
    const to = time + cycles * (1 - playhead);

    try {
      // Query all events (not just onsets) for visualization
      const events = currentPattern.query(from, to) as PianorollHap[];
      pianorollHaps = events;
    } catch (e) {
      // Ignore query errors
    }

    // Draw the pianoroll
    drawPianoroll(pianorollCtx, time, pianorollHaps, {
      cycles,
      playhead,
      fold: true,
      autorange: true,
      active: '#FFCA28',
      inactive: 'rgba(116, 145, 210, 0.6)',
      playheadColor: 'rgba(255, 255, 255, 0.8)',
    });

    pianorollFrameId = requestAnimationFrame(animate);
  };

  pianorollFrameId = requestAnimationFrame(animate);
}

// Stop the pianoroll animation loop
function stopPianorollLoop() {
  if (pianorollFrameId !== null) {
    cancelAnimationFrame(pianorollFrameId);
    pianorollFrameId = null;
  }

  // Clear the canvas
  if (pianorollCtx && pianorollCanvas) {
    pianorollCtx.clearRect(0, 0, pianorollCanvas.width, pianorollCanvas.height);
  }
}

// Set up inline widgets from WASM widget registry
function setupInlineWidgets(wasmWidgets: WasmWidgetConfig[]) {
  // Convert WASM widget configs to CodeMirror widget configs
  const cmConfigs: WidgetConfig[] = wasmWidgets.map((w, index) => {
    // Pre-create the canvas for this widget
    const canvas = getCanvasWidget(`widget_${w.type}_${index}`, {
      width: 400,
      height: 60,
    });

    // Store widget state
    const ctx = canvas.getContext('2d');
    if (ctx) {
      inlineWidgets.set(w.id, {
        id: w.id,
        type: w.type,
        canvas,
        ctx,
        frameId: null,
      });
    }

    return {
      type: w.type,
      to: w.end, // Position widget after the expression
    };
  });

  // Update CodeMirror decorations
  if (cmConfigs.length > 0) {
    updateWidgets(editor, cmConfigs);
  }
}

// Start animation loops for all inline widgets
function startInlineWidgetLoops() {
  for (const widget of inlineWidgets.values()) {
    if (widget.frameId !== null) continue;
    if (widget.type === 'pianoroll') {
      startInlinePianorollLoop(widget);
    } else if (widget.type === 'scope') {
      startInlineScopeLoop(widget);
    }
    // TODO: Add meter loop
  }
}

// Start a pianoroll animation for an inline widget
function startInlinePianorollLoop(widget: InlineWidget) {
  const cycles = 4;
  const playhead = 0.5;

  const animate = () => {
    if (!currentPattern || !scheduler.isRunning) {
      widget.frameId = null;
      return;
    }

    const time = audio.currentTime;
    const from = time - cycles * playhead;
    const to = time + cycles * (1 - playhead);

    try {
      const rawEvents = currentPattern.query(from, to) as unknown[];
      // WASM returns Maps, convert to plain objects and filter by widget tag
      const allEvents = rawEvents.map((e: unknown) => {
        if (e instanceof Map) {
          return {
            start: e.get('start') as number,
            end: e.get('end') as number,
            whole_start: e.get('whole_start') as number | undefined,
            whole_end: e.get('whole_end') as number | undefined,
            value: e.get('value'),
            tags: e.get('tags') as string[] | undefined,
          };
        }
        return e as PianorollHap & { tags?: string[] };
      });

      // Filter by widget tag if tags are available
      const events: PianorollHap[] = allEvents.filter((e) => {
        // If no tags, include all events (fallback behavior)
        if (!e.tags || e.tags.length === 0) return true;
        return e.tags.includes(widget.id);
      });

      drawPianoroll(widget.ctx, time, events, {
        cycles,
        playhead,
        fold: true,
        autorange: true,
        active: '#FFCA28',
        inactive: 'rgba(116, 145, 210, 0.6)',
        playheadColor: 'rgba(255, 255, 255, 0.8)',
      });
    } catch (e) {
      // Ignore query errors
    }

    widget.frameId = requestAnimationFrame(animate);
  };

  widget.frameId = requestAnimationFrame(animate);
}

// Start a scope animation for an inline widget
function startInlineScopeLoop(widget: InlineWidget) {
  const cycles = 4;
  const playhead = 0.5;

  const animate = () => {
    if (!currentPattern || !scheduler.isRunning) {
      widget.frameId = null;
      return;
    }

    const time = audio.currentTime;
    const from = time - cycles * playhead;
    const to = time + cycles * (1 - playhead);

    try {
      const rawEvents = currentPattern.query(from, to) as unknown[];
      // WASM returns Maps, convert to plain objects and filter by widget tag
      const events: ScopeHap[] = rawEvents
        .map((e: unknown) => {
          if (e instanceof Map) {
            return {
              start: e.get('start') as number,
              end: e.get('end') as number,
              whole_start: e.get('whole_start') as number | undefined,
              whole_end: e.get('whole_end') as number | undefined,
              value: e.get('value'),
              tags: e.get('tags') as string[] | undefined,
            };
          }
          return e as ScopeHap & { tags?: string[] };
        })
        .filter((e) => {
          // If no tags, include all events (fallback behavior)
          if (!e.tags || e.tags.length === 0) return true;
          return e.tags.includes(widget.id);
        });

      drawScope(widget.ctx, time, events, {
        cycles,
        playhead,
        autorange: true,
        bipolar: false,
        fill: true,
        dots: true,
        activeColor: '#00FFAA',
        inactiveColor: 'rgba(68, 136, 170, 0.6)',
        playheadColor: 'rgba(255, 255, 255, 0.8)',
      });
    } catch (e) {
      // Ignore query errors
    }

    widget.frameId = requestAnimationFrame(animate);
  };

  widget.frameId = requestAnimationFrame(animate);
}

// Stop all inline widget animation loops
function stopInlineWidgetLoops() {
  for (const widget of inlineWidgets.values()) {
    if (widget.frameId !== null) {
      cancelAnimationFrame(widget.frameId);
      widget.frameId = null;
    }
    // Clear the canvas
    widget.ctx.clearRect(0, 0, widget.canvas.width, widget.canvas.height);
  }
}

function setupUI() {
  document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
    <div class="container">
      <header>
        <h1>crumble</h1>
        <p class="subtitle">live coding patterns in the browser</p>
      </header>

      <div class="controls">
        <button id="play-btn" class="btn btn-primary">Play</button>
        <button id="stop-btn" class="btn btn-secondary">Stop</button>

        <div class="control-group">
          <label for="tempo-slider">BPM:</label>
          <input type="range" id="tempo-slider" min="60" max="200" value="120" />
          <span id="tempo-value">120</span>
        </div>

        <div class="control-group">
          <label for="volume-slider">Vol:</label>
          <input type="range" id="volume-slider" min="0" max="1" step="0.1" value="0.3" />
        </div>

        <div class="control-group export-group">
          <label for="cycles-input">Cycles:</label>
          <input type="number" id="cycles-input" min="1" max="64" value="4" />
          <button id="export-btn" class="btn btn-secondary">Export WAV</button>
        </div>
      </div>

      <div id="editor" class="editor"></div>

      <div id="pianoroll-container" class="pianoroll-container">
        <canvas id="pianoroll"></canvas>
      </div>

      <div class="status-bar">
        <span id="status">Loading...</span>
        <span class="shortcuts">Ctrl+Enter: evaluate | Ctrl+.: stop</span>
      </div>

      <div class="help">
        <h3>Quick Reference</h3>
        <ul>
          <li><code>(seq a b c)</code> - sequence patterns</li>
          <li><code>[a, b, c]</code> - stack/chord (sugar for stack)</li>
          <li><code>~</code> - rest/silence</li>
          <li><code>(fast n pat)</code> - speed up</li>
          <li><code>(slow n pat)</code> - slow down</li>
          <li><code>(euclid k n val)</code> - euclidean rhythm</li>
          <li><code>(rev pat)</code> - reverse</li>
        </ul>
        <h3>Chords</h3>
        <ul>
          <li><code>(chord c4 :major)</code> - C major triad</li>
          <li><code>(chord a3 :minor7)</code> - A minor 7th</li>
          <li><code>(chord d4 :dom7)</code> - D dominant 7th</li>
          <li><code>(chord e4 :sus4)</code> - E suspended 4th</li>
        </ul>
        <h3>Effects</h3>
        <ul>
          <li><code>(delay amt pat)</code> - delay send (0-1)</li>
          <li><code>(room amt pat)</code> - reverb send (0-1)</li>
          <li><code>(drive amt pat)</code> - saturation (0-1)</li>
          <li><code>(lpf hz pat)</code> - low-pass filter</li>
          <li><code>(hpf hz pat)</code> - high-pass filter</li>
          <li><code>(pan pos pat)</code> - stereo pan (-1 to 1)</li>
        </ul>
        <h3>Widgets</h3>
        <ul>
          <li><code>(pianoroll pat)</code> - inline piano roll visualization</li>
          <li><code>(scope pat)</code> - inline waveform/oscilloscope visualization</li>
        </ul>
      </div>
    </div>
  `;
}

function setupEditor() {
  const editorContainer = document.getElementById('editor')!;

  // Default pattern - Hyper Light Drifter inspired synthscape
  const defaultCode = `; crumble - live coding patterns
; Ctrl+Enter to play | Ctrl+. to stop

; -- hyper light drift --
; wrap patterns in (pianoroll ...) or (scope ...) for inline visualization!

(stack
  ; crystalline lead melody - the drifter's theme
  (pianoroll
    (room 0.6 (delay 0.5
      (lpq 4 (lpf 2400 (gain 0.7
        (slow 4 (seq
          e5 ~ g5 b5 ~ ~ d5 ~
          b4 ~ e5 ~ g5 ~ ~ ~))))))))

  ; LFO modulation visualized as scope (for continuous values)
  (scope (slow 4 (sine)))

  ; warm chord bed - Em Cmaj7 G D
  (room 0.5 (lpf 1400 (gain 0.3
    (slow 4 (seq
      (chord e3 :m7)
      (chord c3 :maj7)
      (chord g3 :major)
      (chord d3 :major))))))

  ; shimmering arpeggio
  (pan -0.3 (room 0.5 (delay 0.4
    (lpf 3000 (gain 0.35
      (fast 2 (seq e5 b4 ~ g4 b4 ~ d5 ~)))))))

  ; slow 808 bass - sparse and heavy
  (drive 0.3 (lpf 180 (comp -24
    (slow 2 (seq e2 ~ ~ e2 ~ g2 ~ d2)))))

  ; distant pad texture
  (pan 0.4 (room 0.8
    (lpq 2 (lpf 800 (gain 0.2
      (slow 8 (seq
        [e3, b3] ~ [g3, d4] ~
        [c3, g3] ~ [d3, a3] ~)))))))
)`;

  // Debounced auto-evaluate for live coding
  const debouncedEvaluate = debounce(() => {
    if (scheduler.isRunning) {
      evaluateCode();
    }
  }, 200);

  editor = new EditorView({
    doc: defaultCode,
    extensions: [
      basicSetup,
      StreamLanguage.define(scheme),
      monospaceTheme,
      syntaxHighlighting(monospaceHighlight),
      EditorView.lineWrapping,
      // Draggable numbers
      numberDragPlugin,
      numberDragTheme,
      // Active event highlighting (Strudel-style two-phase approach)
      highlightExtension,
      // Inline widgets (pianoroll, scope, etc.)
      ...widgetExtension,
      keymap.of([
        {
          key: 'Ctrl-Enter',
          mac: 'Cmd-Enter',
          run: () => {
            evaluateCode();
            return true;
          },
        },
        {
          key: 'Ctrl-.',
          mac: 'Cmd-.',
          run: () => {
            stopPlayback();
            return true;
          },
        },
      ]),
      // Auto-evaluate on document changes while playing
      EditorView.updateListener.of((update: ViewUpdate) => {
        if (update.docChanged) {
          debouncedEvaluate();
        }
      }),
    ],
    parent: editorContainer,
  });
}

function setupControls() {
  const playBtn = document.getElementById('play-btn')!;
  const stopBtn = document.getElementById('stop-btn')!;
  const tempoSlider = document.getElementById('tempo-slider') as HTMLInputElement;
  const tempoValue = document.getElementById('tempo-value')!;
  const volumeSlider = document.getElementById('volume-slider') as HTMLInputElement;
  const cyclesInput = document.getElementById('cycles-input') as HTMLInputElement;
  const exportBtn = document.getElementById('export-btn')!;

  // Mobile: ensure audio context can be resumed on any touch
  const resumeAudio = async () => {
    if (audio) {
      await audio.ensureRunning();
    }
  };
  document.addEventListener('touchstart', resumeAudio, { once: false, passive: true });
  document.addEventListener('touchend', resumeAudio, { once: false, passive: true });

  playBtn.addEventListener('click', async () => {
    await startPlayback();
  });

  // Also handle touch for mobile
  playBtn.addEventListener('touchend', async (e) => {
    e.preventDefault();
    await startPlayback();
  });

  stopBtn.addEventListener('click', () => {
    stopPlayback();
  });

  stopBtn.addEventListener('touchend', (e) => {
    e.preventDefault();
    stopPlayback();
  });

  tempoSlider.addEventListener('input', () => {
    const bpm = parseInt(tempoSlider.value);
    tempoValue.textContent = `${bpm}`;
    // Convert BPM to CPS (assuming 4 beats per cycle)
    scheduler.cps = bpm / 60 / 4;
  });

  volumeSlider.addEventListener('input', () => {
    audio.setVolume(parseFloat(volumeSlider.value));
  });

  exportBtn.addEventListener('click', async () => {
    await exportWAV(parseInt(cyclesInput.value) || 4);
  });
}

async function startPlayback() {
  // Initialize audio on first play (requires user gesture)
  // Always call init() - it handles resuming on mobile
  await audio.init();

  // Always evaluate current code
  evaluateCode();

  if (currentPattern) {
    if (!scheduler.isRunning) {
      scheduler.start();
      // Start the animation loops
      startHighlightLoop();
      startPianorollLoop();
      startInlineWidgetLoops();
    } else {
      // Re-evaluate might have set up new widgets
      startInlineWidgetLoops();
    }
    updateStatus('Playing...');
    document.getElementById('play-btn')!.textContent = 'Update';
  }
}

function stopPlayback() {
  scheduler.stop();
  // Stop the animation loops
  stopHighlightLoop();
  stopPianorollLoop();
  stopInlineWidgetLoops();
  updateStatus('Stopped');
  document.getElementById('play-btn')!.textContent = 'Play';
}

function evaluateCode() {
  const code = editor.state.doc.toString();

  if (!code.trim()) {
    updateStatus('No code to evaluate');
    return;
  }

  try {
    currentPattern = evalLisp(code);
    updateStatus('Evaluated');

    // Register all source locations for highlighting (phase 1 of Strudel approach)
    try {
      const locations = currentPattern.getAllLocations() as HighlightLocation[];
      if (locations && locations.length > 0) {
        updateAllLocations(editor, locations);
        console.log(`Registered ${locations.length} source locations for highlighting`);
      }
    } catch (e) {
      console.warn('Could not get locations for highlighting:', e);
    }

    // Get and set up inline widgets
    try {
      const rawWidgets = getWidgets() as unknown[];
      // WASM returns Maps, convert to plain objects
      const widgets: WasmWidgetConfig[] = rawWidgets.map((w: unknown) => {
        if (w instanceof Map) {
          return {
            type: w.get('type') as 'pianoroll' | 'scope' | 'meter',
            id: w.get('id') as string,
            start: w.get('start') as number,
            end: w.get('end') as number,
          };
        }
        return w as WasmWidgetConfig;
      });
      if (widgets && widgets.length > 0) {
        // Clear previous inline widgets
        stopInlineWidgetLoops();
        inlineWidgets.clear();

        setupInlineWidgets(widgets);

        // Start widget animations if already playing
        if (scheduler.isRunning) {
          startInlineWidgetLoops();
        }
      }
    } catch (e) {
      console.warn('Could not get widgets:', e);
    }

    // Set up the query function for the scheduler
    scheduler.setPattern((start: number, end: number): SoundEvent[] => {
      if (!currentPattern) return [];

      try {
        const events = currentPattern.queryOnsets(start, end) as SoundEvent[];
        return events;
      } catch (e) {
        console.error('Query error:', e);
        return [];
      }
    });

    // If already playing, the new pattern will be picked up automatically
    if (!scheduler.isRunning && audio.isRunning) {
      scheduler.start();
    }
  } catch (e) {
    updateStatus(`Error: ${e}`);
    console.error('Evaluation error:', e);
  }
}

function updateStatus(message: string) {
  const status = document.getElementById('status')!;
  status.textContent = message;
}

async function exportWAV(cycles: number) {
  // First evaluate the current code
  const code = editor.state.doc.toString();
  if (!code.trim()) {
    updateStatus('No code to export');
    return;
  }

  let pattern: JsPattern;
  try {
    pattern = evalLisp(code);
  } catch (e) {
    updateStatus(`Error: ${e}`);
    return;
  }

  updateStatus(`Rendering ${cycles} cycles...`);

  const exportBtn = document.getElementById('export-btn')!;
  exportBtn.textContent = 'Rendering...';
  (exportBtn as HTMLButtonElement).disabled = true;

  try {
    // Get current tempo
    const tempoSlider = document.getElementById('tempo-slider') as HTMLInputElement;
    const bpm = parseInt(tempoSlider.value);
    const cps = bpm / 60 / 4;

    // Create offline renderer
    const renderer = new OfflineRenderer({
      cycles,
      cps,
      sampleRate: 44100,
      gain: 0.3,
    });

    // Query function for the pattern
    const queryFn = (start: number, end: number): SoundEvent[] => {
      try {
        return pattern.queryOnsets(start, end) as SoundEvent[];
      } catch (e) {
        console.error('Query error:', e);
        return [];
      }
    };

    // Render
    const buffer = await renderer.render(queryFn);

    // Encode to WAV
    const wav = encodeWAV(buffer);

    // Generate filename with timestamp
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, 19);
    const filename = `crumble-${timestamp}.wav`;

    // Download
    downloadBlob(wav, filename);

    const duration = cycles / cps;
    updateStatus(`Exported ${formatDuration(duration)} to ${filename}`);
  } catch (e) {
    updateStatus(`Export error: ${e}`);
    console.error('Export error:', e);
  } finally {
    exportBtn.textContent = 'Export WAV';
    (exportBtn as HTMLButtonElement).disabled = false;
  }
}

// Start the app
initApp().catch(console.error);
