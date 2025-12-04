import { EditorView, basicSetup } from 'codemirror';
import { keymap } from '@codemirror/view';
import { HighlightStyle, syntaxHighlighting, StreamLanguage } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import init, { evalLisp, JsPattern } from 'crumble';
import { AudioEngine, PatternScheduler } from './audio';
import type { SoundEvent } from './audio';
import './style.css';

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

// Initialize WASM and audio
async function initApp() {
  // Initialize WASM module
  await init();
  console.log('crumble WASM initialized');

  // Create audio engine (will be started on user interaction)
  audio = new AudioEngine({ gain: 0.3 });
  scheduler = new PatternScheduler(audio);
  scheduler.cps = 0.5; // Default tempo

  // Set up UI
  setupUI();
  setupEditor();
  setupControls();

  // Update status
  updateStatus('Ready. Press Ctrl+Enter to evaluate.');
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
      </div>

      <div id="editor" class="editor"></div>

      <div class="status-bar">
        <span id="status">Loading...</span>
        <span class="shortcuts">Ctrl+Enter: evaluate | Ctrl+.: stop</span>
      </div>

      <div class="help">
        <h3>Quick Reference</h3>
        <ul>
          <li><code>(seq a b c)</code> - sequence patterns</li>
          <li><code>(stack a b)</code> - layer patterns</li>
          <li><code>(fast n pat)</code> - speed up</li>
          <li><code>(slow n pat)</code> - slow down</li>
          <li><code>(euclid k n val)</code> - euclidean rhythm</li>
          <li><code>(rev pat)</code> - reverse</li>
        </ul>
      </div>
    </div>
  `;
}

function setupEditor() {
  const editorContainer = document.getElementById('editor')!;

  // Default pattern
  const defaultCode = `; Welcome to crumble!
; A live coding environment for algorithmic music.
;
; Ctrl+Enter (Cmd+Enter on Mac) - evaluate & play
; Ctrl+. (Cmd+. on Mac) - stop

(slow 2 (seq c4 e4 g4 c5 b4 g4 e4 c4))`;

  editor = new EditorView({
    doc: defaultCode,
    extensions: [
      basicSetup,
      StreamLanguage.define(scheme),
      monospaceTheme,
      syntaxHighlighting(monospaceHighlight),
      EditorView.lineWrapping,
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

  playBtn.addEventListener('click', async () => {
    await startPlayback();
  });

  stopBtn.addEventListener('click', () => {
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
}

async function startPlayback() {
  // Initialize audio on first play (requires user gesture)
  if (!audio.isRunning) {
    await audio.init();
  }

  // Evaluate current code if no pattern
  if (!currentPattern) {
    evaluateCode();
  }

  if (currentPattern) {
    scheduler.start();
    updateStatus('Playing...');
    document.getElementById('play-btn')!.textContent = 'Update';
  }
}

function stopPlayback() {
  scheduler.stop();
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

// Start the app
initApp().catch(console.error);
