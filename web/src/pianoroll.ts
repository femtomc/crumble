/**
 * Pianoroll visualization for pattern events.
 *
 * Based on Strudel's pianoroll implementation.
 * Renders a scrolling view of pattern events on a canvas.
 */

// Types
export interface PianorollHap {
  start: number;
  end: number;
  whole_start?: number;
  whole_end?: number;
  value: unknown;
}

export interface PianorollOptions {
  // Display
  cycles?: number;
  playhead?: number;
  vertical?: boolean;
  flipTime?: boolean;
  flipValues?: boolean;

  // Appearance
  active?: string;
  inactive?: string;
  background?: string;
  playheadColor?: string;

  // Note range
  minMidi?: number;
  maxMidi?: number;
  autorange?: boolean;
  fold?: boolean;

  // Features
  labels?: boolean;
  fill?: boolean;
  stroke?: boolean;
  hideInactive?: boolean;
}

const DEFAULT_OPTIONS: Required<PianorollOptions> = {
  cycles: 4,
  playhead: 0.5,
  vertical: false,
  flipTime: false,
  flipValues: false,
  active: '#FFCA28',
  inactive: '#7491D2',
  background: 'transparent',
  playheadColor: '#ffffff',
  minMidi: 36,
  maxMidi: 84,
  autorange: true,
  fold: true,
  labels: false,
  fill: true,
  stroke: false,
  hideInactive: false,
};

/**
 * Convert a note name to MIDI number.
 */
function noteToMidi(note: string): number {
  const noteMap: Record<string, number> = {
    c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11,
  };

  const match = note.toLowerCase().match(/^([a-g])([#b]?)(-?\d+)?$/);
  if (!match) return 60; // Default to middle C

  const [, noteName, accidental, octaveStr] = match;
  const octave = octaveStr ? parseInt(octaveStr) : 4;
  let midi = noteMap[noteName] + (octave + 1) * 12;

  if (accidental === '#') midi += 1;
  if (accidental === 'b') midi -= 1;

  return midi;
}

/**
 * Extract MIDI value from a hap.
 */
function getValue(hap: PianorollHap): number | string {
  const value = hap.value;

  if (typeof value === 'number') {
    return value;
  }

  if (typeof value === 'string') {
    // Try to parse as note name
    if (/^[a-g][#b]?-?\d*$/i.test(value)) {
      return noteToMidi(value);
    }
    // Return string for non-note values (samples, etc.)
    return value;
  }

  if (typeof value === 'object' && value !== null) {
    const v = value as Record<string, unknown>;
    if (typeof v.note === 'string') return noteToMidi(v.note);
    if (typeof v.note === 'number') return v.note;
    if (typeof v.n === 'string') return noteToMidi(v.n);
    if (typeof v.n === 'number') return v.n;
    if (typeof v.freq === 'number') return Math.round(12 * Math.log2(v.freq / 440) + 69);
    if (typeof v.s === 'string') return '_' + v.s;
  }

  return 60; // Default
}

/**
 * Linear interpolation.
 */
function scale(normalized: number, min: number, max: number): number {
  return normalized * (max - min) + min;
}

/**
 * Draw a pianoroll visualization.
 */
export function drawPianoroll(
  ctx: CanvasRenderingContext2D,
  time: number,
  haps: PianorollHap[],
  options: PianorollOptions = {}
): void {
  const opts = { ...DEFAULT_OPTIONS, ...options };
  const {
    cycles,
    playhead,
    vertical,
    flipTime,
    flipValues,
    active,
    inactive,
    background,
    playheadColor,
    fold,
    labels,
    fill,
    stroke,
    hideInactive,
  } = opts;
  let { minMidi, maxMidi, autorange } = opts;

  const w = ctx.canvas.width;
  const h = ctx.canvas.height;
  const pixelRatio = window.devicePixelRatio || 1;

  // Time window
  const from = -cycles * playhead;
  const to = cycles * (1 - playhead);

  // Axes
  const timeAxis = vertical ? h : w;
  const valueAxis = vertical ? w : h;
  let timeRange: [number, number] = vertical ? [timeAxis, 0] : [0, timeAxis];
  const timeExtent = to - from;
  let valueRange: [number, number] = vertical ? [0, valueAxis] : [valueAxis, 0];

  if (flipTime) timeRange = [timeRange[1], timeRange[0]];
  if (flipValues) valueRange = [valueRange[1], valueRange[0]];

  // Collect values for autorange/fold
  const values: (number | string)[] = [];
  let minVal = Infinity;
  let maxVal = -Infinity;

  for (const hap of haps) {
    const v = getValue(hap);
    if (!values.includes(v)) values.push(v);
    if (typeof v === 'number') {
      minVal = Math.min(minVal, v);
      maxVal = Math.max(maxVal, v);
    }
  }

  // Sort values
  values.sort((a, b) => {
    if (typeof a === 'number' && typeof b === 'number') return a - b;
    if (typeof a === 'number') return -1;
    if (typeof b === 'number') return 1;
    return String(a).localeCompare(String(b));
  });

  if (autorange && minVal !== Infinity) {
    minMidi = minVal;
    maxMidi = maxVal;
  }

  const valueExtent = fold ? values.length : maxMidi - minMidi + 1;
  const barThickness = valueAxis / Math.max(valueExtent, 1);

  // Clear canvas
  ctx.fillStyle = background;
  ctx.clearRect(0, 0, w, h);
  if (background !== 'transparent') {
    ctx.fillRect(0, 0, w, h);
  }

  // Draw events
  for (const hap of haps) {
    const hapStart = hap.whole_start ?? hap.start;
    const hapEnd = hap.whole_end ?? hap.end;
    const duration = hapEnd - hapStart;

    const isActive = hapStart <= time && hapEnd > time;

    if (hideInactive && !isActive) continue;

    const color = isActive ? active : inactive;
    ctx.fillStyle = fill ? color : 'transparent';
    ctx.strokeStyle = color;
    ctx.globalAlpha = isActive ? 1 : 0.6;

    // Time position
    const timeProgress = (hapStart - (flipTime ? to : from)) / timeExtent;
    const timePx = scale(timeProgress, ...timeRange);
    const durationPx = scale(duration / timeExtent, 0, timeAxis);

    // Value position
    const value = getValue(hap);
    const valueProgress = fold
      ? values.indexOf(value) / Math.max(values.length, 1)
      : (typeof value === 'number' ? (value - minMidi) / valueExtent : 0.5);
    const valuePx = scale(valueProgress, ...valueRange);

    // Offset for current time
    const offset = scale(time / timeExtent, ...timeRange);

    // Calculate rectangle coordinates
    let coords: [number, number, number, number];
    if (vertical) {
      coords = [
        valuePx + 1 - (flipValues ? barThickness : 0),
        timeAxis - offset + timePx + 1 - (flipTime ? 0 : durationPx),
        barThickness - 2,
        durationPx - 2,
      ];
    } else {
      coords = [
        timePx - offset + 1 - (flipTime ? durationPx : 0),
        valuePx + 1 - (flipValues ? 0 : barThickness),
        durationPx - 2,
        barThickness - 2,
      ];
    }

    // Draw
    if (fill) {
      ctx.fillRect(...coords);
    }
    if (stroke || isActive) {
      ctx.strokeRect(...coords);
    }

    // Labels
    if (labels && typeof value === 'string') {
      const fontSize = Math.min(barThickness * 0.8, 14 * pixelRatio);
      ctx.font = `${fontSize}px monospace`;
      ctx.fillStyle = isActive ? '#000' : color;
      ctx.textBaseline = 'top';
      ctx.fillText(value, coords[0] + 2, coords[1] + 2);
    }
  }

  ctx.globalAlpha = 1;

  // Draw playhead
  ctx.strokeStyle = playheadColor;
  ctx.lineWidth = 2 * pixelRatio;
  ctx.beginPath();
  const playheadPosition = scale(-from / timeExtent, ...timeRange);
  if (vertical) {
    ctx.moveTo(0, playheadPosition);
    ctx.lineTo(valueAxis, playheadPosition);
  } else {
    ctx.moveTo(playheadPosition, 0);
    ctx.lineTo(playheadPosition, valueAxis);
  }
  ctx.stroke();
  ctx.lineWidth = 1;
}

/**
 * Animation frame manager for pianoroll.
 */
export class PianorollAnimator {
  private frameId: number | null = null;
  private ctx: CanvasRenderingContext2D;
  private options: PianorollOptions;
  private getTime: () => number;
  private getHaps: (from: number, to: number) => PianorollHap[];

  constructor(
    canvas: HTMLCanvasElement,
    options: PianorollOptions,
    getTime: () => number,
    getHaps: (from: number, to: number) => PianorollHap[]
  ) {
    this.ctx = canvas.getContext('2d')!;
    this.options = options;
    this.getTime = getTime;
    this.getHaps = getHaps;
  }

  start(): void {
    if (this.frameId !== null) return;

    const animate = () => {
      const time = this.getTime();
      const cycles = this.options.cycles ?? 4;
      const playhead = this.options.playhead ?? 0.5;

      // Query haps for visible time range
      const from = time - cycles * playhead;
      const to = time + cycles * (1 - playhead);
      const haps = this.getHaps(from, to);

      drawPianoroll(this.ctx, time, haps, this.options);

      this.frameId = requestAnimationFrame(animate);
    };

    this.frameId = requestAnimationFrame(animate);
  }

  stop(): void {
    if (this.frameId !== null) {
      cancelAnimationFrame(this.frameId);
      this.frameId = null;
    }
  }

  setOptions(options: Partial<PianorollOptions>): void {
    this.options = { ...this.options, ...options };
  }
}
