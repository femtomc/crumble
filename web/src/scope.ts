/**
 * Scope (waveform) visualization for pattern events.
 *
 * Renders a scrolling oscilloscope-style view of pattern values.
 * Unlike pianoroll which shows discrete notes, scope shows continuous
 * waveform-like visualization of numeric values over time.
 */

// Types
export interface ScopeHap {
  start: number;
  end: number;
  whole_start?: number;
  whole_end?: number;
  value: unknown;
}

export interface ScopeOptions {
  // Display
  cycles?: number;
  playhead?: number;
  flipTime?: boolean;
  flipValues?: boolean;

  // Appearance
  activeColor?: string;
  inactiveColor?: string;
  background?: string;
  playheadColor?: string;
  lineWidth?: number;

  // Value range
  minValue?: number;
  maxValue?: number;
  autorange?: boolean;
  bipolar?: boolean; // If true, range is -1 to 1 centered at 0

  // Features
  fill?: boolean; // Fill area under curve
  dots?: boolean; // Show dots at event points
}

const DEFAULT_OPTIONS: Required<ScopeOptions> = {
  cycles: 4,
  playhead: 0.5,
  flipTime: false,
  flipValues: false,
  activeColor: '#00FFAA',
  inactiveColor: '#4488AA',
  background: 'transparent',
  playheadColor: '#ffffff',
  lineWidth: 2,
  minValue: 0,
  maxValue: 1,
  autorange: true,
  bipolar: false,
  fill: true,
  dots: false,
};

/**
 * Convert a note name to MIDI number.
 */
function noteToMidi(note: string): number | null {
  const noteMap: Record<string, number> = {
    c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11,
  };

  const match = note.toLowerCase().match(/^([a-g])([#b]?)(-?\d+)?$/);
  if (!match) return null;

  const [, noteName, accidental, octaveStr] = match;
  const octave = octaveStr ? parseInt(octaveStr) : 4;
  let midi = noteMap[noteName] + (octave + 1) * 12;

  if (accidental === '#') midi += 1;
  if (accidental === 'b') midi -= 1;

  return midi;
}

/**
 * Extract numeric value from a hap.
 */
function getValue(hap: ScopeHap): number | null {
  const value = hap.value;

  if (typeof value === 'number') {
    return value;
  }

  if (typeof value === 'string') {
    // Try to parse as note name (e.g., "c4", "e2", "g#3")
    return noteToMidi(value);
  }

  if (typeof value === 'object' && value !== null) {
    const v = value as Record<string, unknown>;
    // Check for common value properties
    if (typeof v.value === 'number') return v.value;
    if (typeof v.n === 'number') return v.n;
    if (typeof v.note === 'string') return noteToMidi(v.note);
    if (typeof v.note === 'number') return v.note;
    if (typeof v.freq === 'number') return v.freq;
    if (typeof v.gain === 'number') return v.gain;
  }

  return null;
}

/**
 * Linear interpolation.
 */
function scale(normalized: number, min: number, max: number): number {
  return normalized * (max - min) + min;
}

/**
 * Draw a scope (waveform) visualization.
 */
export function drawScope(
  ctx: CanvasRenderingContext2D,
  time: number,
  haps: ScopeHap[],
  options: ScopeOptions = {}
): void {
  const opts = { ...DEFAULT_OPTIONS, ...options };
  const {
    cycles,
    playhead,
    flipTime,
    flipValues,
    activeColor,
    inactiveColor,
    background,
    playheadColor,
    lineWidth,
    bipolar,
    fill,
    dots,
  } = opts;
  let { minValue, maxValue, autorange } = opts;

  const w = ctx.canvas.width;
  const h = ctx.canvas.height;
  const pixelRatio = window.devicePixelRatio || 1;

  // Time window
  const from = time - cycles * playhead;
  const to = time + cycles * (1 - playhead);
  const timeExtent = to - from;

  // Collect numeric values for autorange
  const numericHaps: { hap: ScopeHap; value: number }[] = [];
  let minVal = Infinity;
  let maxVal = -Infinity;

  for (const hap of haps) {
    const v = getValue(hap);
    if (v !== null) {
      numericHaps.push({ hap, value: v });
      minVal = Math.min(minVal, v);
      maxVal = Math.max(maxVal, v);
    }
  }

  // Sort by start time
  numericHaps.sort((a, b) => a.hap.start - b.hap.start);

  if (autorange && numericHaps.length > 0) {
    if (bipolar) {
      // Center at 0, extend to cover both sides
      const extent = Math.max(Math.abs(minVal), Math.abs(maxVal));
      minValue = -extent;
      maxValue = extent;
    } else {
      // Add some padding
      const padding = (maxVal - minVal) * 0.1 || 0.5;
      minValue = minVal - padding;
      maxValue = maxVal + padding;
    }
  } else if (bipolar) {
    minValue = -1;
    maxValue = 1;
  }

  const valueExtent = maxValue - minValue;

  // Clear canvas
  ctx.fillStyle = background;
  ctx.clearRect(0, 0, w, h);
  if (background !== 'transparent') {
    ctx.fillRect(0, 0, w, h);
  }

  // Draw center line for bipolar mode
  if (bipolar) {
    const centerY = h / 2;
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.2)';
    ctx.lineWidth = 1 * pixelRatio;
    ctx.beginPath();
    ctx.moveTo(0, centerY);
    ctx.lineTo(w, centerY);
    ctx.stroke();
  }

  // No events to draw
  if (numericHaps.length === 0) {
    // Draw playhead only
    ctx.strokeStyle = playheadColor;
    ctx.lineWidth = 2 * pixelRatio;
    const playheadX = w * playhead;
    ctx.beginPath();
    ctx.moveTo(playheadX, 0);
    ctx.lineTo(playheadX, h);
    ctx.stroke();
    return;
  }

  // Build path through events
  ctx.lineWidth = lineWidth * pixelRatio;
  ctx.lineCap = 'round';
  ctx.lineJoin = 'round';

  // Calculate positions
  interface Point {
    x: number;
    y: number;
    isActive: boolean;
  }
  const points: Point[] = [];

  for (const { hap, value } of numericHaps) {
    const hapStart = hap.whole_start ?? hap.start;
    const hapEnd = hap.whole_end ?? hap.end;
    const isActive = hapStart <= time && hapEnd > time;

    // Calculate x position relative to playhead
    const timeProgress = (hapStart - from) / timeExtent;
    let x = timeProgress * w;
    if (flipTime) x = w - x;

    // Calculate y position (value)
    const valueProgress = (value - minValue) / valueExtent;
    let y = (1 - valueProgress) * h; // Invert so higher values are at top
    if (flipValues) y = h - y;

    points.push({ x, y, isActive });

    // Add end point for sustained values
    const endTimeProgress = (hapEnd - from) / timeExtent;
    let endX = endTimeProgress * w;
    if (flipTime) endX = w - endX;

    if (Math.abs(endX - x) > 2) {
      points.push({ x: endX, y, isActive });
    }
  }

  // Draw filled area if enabled
  if (fill && points.length > 1) {
    const baseY = bipolar ? h / 2 : h;

    ctx.beginPath();
    ctx.moveTo(points[0].x, baseY);

    for (const point of points) {
      ctx.lineTo(point.x, point.y);
    }

    ctx.lineTo(points[points.length - 1].x, baseY);
    ctx.closePath();

    // Create gradient fill
    const gradient = ctx.createLinearGradient(0, 0, 0, h);
    gradient.addColorStop(0, 'rgba(0, 255, 170, 0.3)');
    gradient.addColorStop(1, 'rgba(0, 255, 170, 0.05)');
    ctx.fillStyle = gradient;
    ctx.fill();
  }

  // Draw line segments with color based on active state
  for (let i = 0; i < points.length - 1; i++) {
    const p1 = points[i];
    const p2 = points[i + 1];

    ctx.strokeStyle = p1.isActive ? activeColor : inactiveColor;
    ctx.globalAlpha = p1.isActive ? 1 : 0.6;

    ctx.beginPath();
    ctx.moveTo(p1.x, p1.y);
    ctx.lineTo(p2.x, p2.y);
    ctx.stroke();
  }

  ctx.globalAlpha = 1;

  // Draw dots at event points
  if (dots) {
    for (const point of points) {
      ctx.fillStyle = point.isActive ? activeColor : inactiveColor;
      ctx.beginPath();
      ctx.arc(point.x, point.y, (point.isActive ? 4 : 3) * pixelRatio, 0, Math.PI * 2);
      ctx.fill();
    }
  }

  // Draw playhead
  ctx.strokeStyle = playheadColor;
  ctx.lineWidth = 2 * pixelRatio;
  const playheadX = w * playhead;
  ctx.beginPath();
  ctx.moveTo(playheadX, 0);
  ctx.lineTo(playheadX, h);
  ctx.stroke();
}

/**
 * Animation frame manager for scope.
 */
export class ScopeAnimator {
  private frameId: number | null = null;
  private ctx: CanvasRenderingContext2D;
  private options: ScopeOptions;
  private getTime: () => number;
  private getHaps: (from: number, to: number) => ScopeHap[];

  constructor(
    canvas: HTMLCanvasElement,
    options: ScopeOptions,
    getTime: () => number,
    getHaps: (from: number, to: number) => ScopeHap[]
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

      drawScope(this.ctx, time, haps, this.options);

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

  setOptions(options: Partial<ScopeOptions>): void {
    this.options = { ...this.options, ...options };
  }
}
