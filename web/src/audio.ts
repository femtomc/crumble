/**
 * Web Audio synthesis for crumble.
 *
 * This module provides a simple synthesizer using the Web Audio API,
 * triggered by pattern events from the crumble WASM module.
 */

export type Waveform = 'sine' | 'saw' | 'sawtooth' | 'square' | 'triangle' | 'noise';

export interface SoundEvent {
  start: number;      // Start time in cycles
  end: number;        // End time in cycles
  value: string | number;
  whole_start?: number;
  whole_end?: number;
}

export interface AudioEngineOptions {
  gain?: number;
  attack?: number;
  decay?: number;
  sustain?: number;
  release?: number;
}

/**
 * Simple Web Audio synthesizer for live coding.
 */
export class AudioEngine {
  private ctx: AudioContext | null = null;
  private masterGain: GainNode | null = null;
  private options: Required<AudioEngineOptions>;

  constructor(options: AudioEngineOptions = {}) {
    this.options = {
      gain: options.gain ?? 0.3,
      attack: options.attack ?? 0.005,
      decay: options.decay ?? 0.1,
      sustain: options.sustain ?? 0.3,
      release: options.release ?? 0.1,
    };
  }

  /**
   * Initialize the audio context (must be called from a user gesture).
   */
  async init(): Promise<void> {
    if (this.ctx) return;

    this.ctx = new AudioContext();
    this.masterGain = this.ctx.createGain();
    this.masterGain.gain.value = this.options.gain;
    this.masterGain.connect(this.ctx.destination);

    // Resume if suspended
    if (this.ctx.state === 'suspended') {
      await this.ctx.resume();
    }
  }

  /**
   * Get the current audio time.
   */
  get currentTime(): number {
    return this.ctx?.currentTime ?? 0;
  }

  /**
   * Check if audio is initialized and running.
   */
  get isRunning(): boolean {
    return this.ctx?.state === 'running';
  }

  /**
   * Play a sound at a specific time.
   */
  playAt(time: number, duration: number, value: string | number): void {
    if (!this.ctx || !this.masterGain) return;

    const { waveform, freq, isKick } = this.parseValue(value);

    if (waveform === 'noise') {
      this.playNoise(time, duration);
    } else if (isKick) {
      this.playKick(time, duration);
    } else {
      this.playTone(time, duration, freq, waveform);
    }
  }

  /**
   * Play immediately.
   */
  playNow(duration: number, value: string | number): void {
    this.playAt(this.currentTime, duration, value);
  }

  /**
   * Set master volume (0-1).
   */
  setVolume(volume: number): void {
    if (this.masterGain) {
      this.masterGain.gain.value = Math.max(0, Math.min(1, volume));
    }
  }

  /**
   * Stop all sounds.
   */
  stop(): void {
    if (this.ctx) {
      this.ctx.close();
      this.ctx = null;
      this.masterGain = null;
    }
  }

  private parseValue(value: string | number): { waveform: OscillatorType | 'noise'; freq: number; isKick: boolean } {
    if (typeof value === 'number') {
      // Number is interpreted as MIDI note or frequency
      const freq = value < 128 ? this.midiToFreq(value) : value;
      return { waveform: 'sine', freq, isKick: false };
    }

    // String values
    const lower = value.toLowerCase();

    // Check for waveform names
    switch (lower) {
      case 'sine':
      case 'sin':
        return { waveform: 'sine', freq: 440, isKick: false };
      case 'saw':
      case 'sawtooth':
        return { waveform: 'sawtooth', freq: 440, isKick: false };
      case 'square':
      case 'sq':
      case 'pulse':
        return { waveform: 'square', freq: 440, isKick: false };
      case 'tri':
      case 'triangle':
        return { waveform: 'triangle', freq: 440, isKick: false };
      case 'noise':
      case 'white':
        return { waveform: 'noise', freq: 0, isKick: false };
    }

    // Check for note names (e.g., "c4", "a#3")
    const noteMatch = lower.match(/^([a-g])([#b]?)(\d+)?$/);
    if (noteMatch) {
      const [, note, accidental, octaveStr] = noteMatch;
      const octave = octaveStr ? parseInt(octaveStr) : 4;
      const midi = this.noteToMidi(note, accidental, octave);
      // Use sawtooth for a richer synth sound
      return { waveform: 'sawtooth', freq: this.midiToFreq(midi), isKick: false };
    }

    // Check for drum sounds
    switch (lower) {
      case 'kick':
      case 'bd':
        return { waveform: 'sine', freq: 150, isKick: true };
      case 'snare':
      case 'sd':
        return { waveform: 'noise', freq: 0, isKick: false };
      case 'hihat':
      case 'hh':
        return { waveform: 'noise', freq: 0, isKick: false };
      case 'clap':
      case 'cp':
        return { waveform: 'noise', freq: 0, isKick: false };
      default:
        return { waveform: 'sine', freq: 440, isKick: false };
    }
  }

  private playTone(time: number, duration: number, freq: number, waveform: OscillatorType): void {
    if (!this.ctx || !this.masterGain) return;

    const osc = this.ctx.createOscillator();
    const gain = this.ctx.createGain();
    const filter = this.ctx.createBiquadFilter();

    osc.type = waveform;
    osc.frequency.value = freq;

    // Low-pass filter for warmth
    filter.type = 'lowpass';
    filter.frequency.setValueAtTime(freq * 4, time);
    filter.frequency.exponentialRampToValueAtTime(freq * 2, time + 0.1);
    filter.Q.value = 1;

    // ADSR envelope
    const { attack, decay, sustain, release } = this.options;
    const attackEnd = time + attack;
    const decayEnd = attackEnd + decay;
    const releaseStart = time + duration - release;

    gain.gain.setValueAtTime(0, time);
    gain.gain.linearRampToValueAtTime(0.6, attackEnd);
    gain.gain.linearRampToValueAtTime(sustain * 0.6, decayEnd);
    gain.gain.setValueAtTime(sustain * 0.6, Math.max(decayEnd, releaseStart));
    gain.gain.linearRampToValueAtTime(0, time + duration);

    osc.connect(filter);
    filter.connect(gain);
    gain.connect(this.masterGain);

    osc.start(time);
    osc.stop(time + duration + 0.1);
  }

  private playKick(time: number, duration: number): void {
    if (!this.ctx || !this.masterGain) return;

    const osc = this.ctx.createOscillator();
    const gain = this.ctx.createGain();

    osc.type = 'sine';

    // Pitch sweep from 150Hz down to 50Hz for that classic kick sound
    osc.frequency.setValueAtTime(150, time);
    osc.frequency.exponentialRampToValueAtTime(50, time + 0.05);

    // Quick attack, exponential decay
    gain.gain.setValueAtTime(1, time);
    gain.gain.exponentialRampToValueAtTime(0.01, time + Math.min(duration, 0.3));

    osc.connect(gain);
    gain.connect(this.masterGain);

    osc.start(time);
    osc.stop(time + Math.min(duration, 0.3) + 0.1);
  }

  private playNoise(time: number, duration: number): void {
    if (!this.ctx || !this.masterGain) return;

    // Create white noise buffer
    const bufferSize = this.ctx.sampleRate * duration;
    const buffer = this.ctx.createBuffer(1, bufferSize, this.ctx.sampleRate);
    const data = buffer.getChannelData(0);

    for (let i = 0; i < bufferSize; i++) {
      data[i] = Math.random() * 2 - 1;
    }

    const source = this.ctx.createBufferSource();
    const gain = this.ctx.createGain();
    const filter = this.ctx.createBiquadFilter();

    source.buffer = buffer;
    filter.type = 'highpass';
    filter.frequency.value = 1000;

    // Short envelope for percussive sounds
    const { attack } = this.options;
    gain.gain.setValueAtTime(0, time);
    gain.gain.linearRampToValueAtTime(0.5, time + attack);
    gain.gain.exponentialRampToValueAtTime(0.01, time + duration);

    source.connect(filter);
    filter.connect(gain);
    gain.connect(this.masterGain);

    source.start(time);
    source.stop(time + duration + 0.1);
  }

  private midiToFreq(midi: number): number {
    return 440 * Math.pow(2, (midi - 69) / 12);
  }

  private noteToMidi(note: string, accidental: string, octave: number): number {
    const noteValues: Record<string, number> = {
      c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11,
    };
    let midi = noteValues[note] + (octave + 1) * 12;
    if (accidental === '#') midi += 1;
    if (accidental === 'b') midi -= 1;
    return midi;
  }
}

/**
 * Scheduler that queries patterns and triggers audio events.
 */
export class PatternScheduler {
  private audio: AudioEngine;
  private running = false;
  private startTime = 0;
  private lastQueryEnd = 0;
  private timerId: number | null = null;

  // Timing configuration
  public cps = 0.5;           // Cycles per second (tempo)
  public lookahead = 0.25;    // How far ahead to schedule (seconds) - increased for stability
  public tickInterval = 25;   // How often to tick (ms) - decreased for tighter timing

  // Query function provided by user
  private queryFn: ((start: number, end: number) => SoundEvent[]) | null = null;

  constructor(audio: AudioEngine) {
    this.audio = audio;
  }

  /**
   * Set the pattern query function.
   */
  setPattern(queryFn: (start: number, end: number) => SoundEvent[]): void {
    this.queryFn = queryFn;
  }

  /**
   * Start the scheduler.
   */
  start(): void {
    if (this.running || !this.queryFn) return;

    this.running = true;
    this.startTime = this.audio.currentTime;
    this.lastQueryEnd = 0;

    // Use setInterval for consistent timing (works even when tab is backgrounded)
    this.tick();
    this.timerId = window.setInterval(this.tick, this.tickInterval);
  }

  /**
   * Stop the scheduler.
   */
  stop(): void {
    this.running = false;
    if (this.timerId !== null) {
      clearInterval(this.timerId);
      this.timerId = null;
    }
  }

  /**
   * Check if running.
   */
  get isRunning(): boolean {
    return this.running;
  }

  private tick = (): void => {
    if (!this.running || !this.queryFn) return;

    const now = this.audio.currentTime - this.startTime;
    const queryEnd = (now + this.lookahead) * this.cps;

    if (queryEnd > this.lastQueryEnd) {
      const queryStart = this.lastQueryEnd;

      // Query the pattern
      const events = this.queryFn(queryStart, queryEnd);

      // Schedule events
      for (const event of events) {
        // Handle both Map objects (from serde_wasm_bindgen) and plain objects
        const start = event instanceof Map ? event.get('start') : event.start;
        const end = event instanceof Map ? event.get('end') : event.end;
        const value = event instanceof Map ? event.get('value') : event.value;
        const whole_start = event instanceof Map ? event.get('whole_start') : event.whole_start;
        const whole_end = event instanceof Map ? event.get('whole_end') : event.whole_end;

        // Only schedule events with onsets in this window
        if (start >= queryStart && start < queryEnd) {
          const triggerTime = this.startTime + start / this.cps;
          const duration = (end - start) / this.cps;

          // Use whole duration if available
          const actualDuration = whole_end && whole_start
            ? (whole_end - whole_start) / this.cps
            : duration;

          this.audio.playAt(triggerTime, actualDuration, value);
        }
      }

      this.lastQueryEnd = queryEnd;
    }
    // tick is called by setInterval, no need to reschedule here
  };
}
