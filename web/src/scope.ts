/**
 * Audio oscilloscope visualization.
 *
 * Renders a real-time waveform display of audio output,
 * similar to Strudel's scope implementation.
 */

export interface ScopeOptions {
  // Waveform display
  align?: boolean;      // Align to zero-crossing for stable display
  trigger?: number;     // Amplitude threshold for alignment (default 0)

  // Appearance
  color?: string;       // Line color (default white)
  thickness?: number;   // Line thickness (default 2)
  scale?: number;       // Y-axis scale factor (default 0.4)
  pos?: number;         // Y position 0=top, 1=bottom (default 0.5)

  // Background
  background?: string;  // Background color (default transparent)
  smear?: number;       // Trail effect 0-1 (0 = clear each frame)
}

const DEFAULT_OPTIONS: Required<ScopeOptions> = {
  align: true,
  trigger: 0,
  color: '#00FFAA',
  thickness: 3,       // Strudel default
  scale: 0.25,        // Strudel default (was 0.4)
  pos: 0.5,           // Center (Strudel uses 0.75)
  background: 'transparent',
  smear: 0,
};

/**
 * Draw an audio oscilloscope waveform.
 *
 * @param ctx - Canvas 2D context
 * @param dataArray - Float32Array of time-domain audio samples (-1 to 1)
 * @param options - Rendering options
 */
export function drawScope(
  ctx: CanvasRenderingContext2D,
  dataArray: Float32Array<ArrayBuffer> | null,
  options: ScopeOptions = {}
): void {
  const opts = { ...DEFAULT_OPTIONS, ...options };
  const {
    align,
    trigger,
    color,
    thickness,
    scale,
    pos,
    background,
    smear,
  } = opts;

  const canvas = ctx.canvas;
  const w = canvas.width;
  const h = canvas.height;
  const pixelRatio = window.devicePixelRatio || 1;

  // Clear or smear background
  if (smear > 0) {
    ctx.fillStyle = `rgba(0, 0, 0, ${1 - smear})`;
    ctx.fillRect(0, 0, w, h);
  } else {
    ctx.clearRect(0, 0, w, h);
    if (background !== 'transparent') {
      ctx.fillStyle = background;
      ctx.fillRect(0, 0, w, h);
    }
  }

  // Set line style
  ctx.strokeStyle = color;
  ctx.lineWidth = thickness * pixelRatio;
  ctx.lineCap = 'round';
  ctx.lineJoin = 'round';

  // If no data, draw a flat line at center
  if (!dataArray || dataArray.length === 0) {
    const y = pos * h;
    ctx.beginPath();
    ctx.moveTo(0, y);
    ctx.lineTo(w, y);
    ctx.stroke();
    return;
  }

  const bufferSize = dataArray.length;

  // Find trigger point for stable waveform alignment
  // Match Strudel: look for positive-to-negative zero crossing
  let triggerIndex = 0;
  if (align) {
    for (let i = 1; i < bufferSize; i++) {
      if (dataArray[i - 1] > -trigger && dataArray[i] <= -trigger) {
        triggerIndex = i;
        break;
      }
    }
  }

  // Draw the waveform (matching Strudel's approach - no wrap-around)
  ctx.beginPath();

  const sliceWidth = w / bufferSize;
  let x = 0;

  for (let i = triggerIndex; i < bufferSize; i++) {
    const sample = dataArray[i];
    // Scale and position: pos is center line, scale controls amplitude
    const y = (pos - scale * sample) * h;

    if (x === 0) {
      ctx.moveTo(x, y);
    } else {
      ctx.lineTo(x, y);
    }
    x += sliceWidth;
  }

  ctx.stroke();
}

/**
 * Draw a frequency spectrum visualization.
 *
 * @param ctx - Canvas 2D context
 * @param dataArray - Float32Array of frequency data (in dB)
 * @param options - Rendering options
 */
export function drawSpectrum(
  ctx: CanvasRenderingContext2D,
  dataArray: Float32Array<ArrayBuffer> | null,
  options: {
    color?: string;
    scale?: number;
    pos?: number;
    min?: number;
    max?: number;
    background?: string;
  } = {}
): void {
  const {
    color = '#00FFAA',
    scale = 0.4,
    pos = 0.75,
    min = -100,
    max = 0,
    background = 'transparent',
  } = options;

  const canvas = ctx.canvas;
  const w = canvas.width;
  const h = canvas.height;

  // Clear background
  ctx.clearRect(0, 0, w, h);
  if (background !== 'transparent') {
    ctx.fillStyle = background;
    ctx.fillRect(0, 0, w, h);
  }

  if (!dataArray || dataArray.length === 0) {
    return;
  }

  ctx.fillStyle = color;
  const bufferSize = dataArray.length;
  const sliceWidth = w / bufferSize;

  let x = 0;
  for (let i = 0; i < bufferSize; i++) {
    // Normalize dB value to 0-1 range
    const normalized = Math.max(0, Math.min(1, (dataArray[i] - min) / (max - min)));
    const barHeight = normalized * scale * h;
    const y = pos * h - barHeight;

    ctx.fillRect(x, y, Math.max(sliceWidth - 1, 1), barHeight);
    x += sliceWidth;
  }
}
