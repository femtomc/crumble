/**
 * Web Audio synthesis for crumble.
 *
 * This module provides a simple synthesizer using the Web Audio API,
 * triggered by pattern events from the crumble WASM module.
 */

export type Waveform = 'sine' | 'saw' | 'sawtooth' | 'square' | 'triangle' | 'noise';

export interface EffectMeta {
  // Effects
  delay?: number;       // Delay time (0-1)
  delayfeedback?: number; // Delay feedback (0-1)
  room?: number;        // Reverb room size (0-1)
  size?: number;        // Reverb mix (0-1)
  drive?: number;       // Saturation amount (0-1)
  gain?: number;        // Per-event gain (0-1)
  pan?: number;         // Pan position (-1 to 1)
  lpf?: number;         // Low-pass filter frequency (Hz)
  hpf?: number;         // High-pass filter frequency (Hz)
  lpq?: number;         // Low-pass filter resonance (Q: 0.001 to 30)
  hpq?: number;         // High-pass filter resonance (Q: 0.001 to 30)
  comp_threshold?: number; // Compressor threshold (dB, -100 to 0)
  comp_ratio?: number;     // Compressor ratio (1 to 20)
  comp_attack?: number;    // Compressor attack (seconds, 0 to 1)
  comp_release?: number;   // Compressor release (seconds, 0 to 1)

  // ADSR envelope (per-note)
  attack?: number;      // Attack time (seconds)
  decay?: number;       // Decay time (seconds)
  sustain?: number;     // Sustain level (0-1)
  release?: number;     // Release time (seconds)

  // FM synthesis
  fm_index?: number;    // FM modulation index (0-100+)
  fm_ratio?: number;    // Modulator:carrier frequency ratio
  fm_wave?: string;     // Modulator waveform (sine, saw, square, tri)

  // Multi-oscillator
  detune?: number;      // Detune in cents
  unison?: number;      // Number of unison voices (1-8)
  wave?: string;        // Explicit waveform override

  // Filter modulation
  filter_env?: number;      // Filter envelope amount (Hz)
  filter_lfo_rate?: number; // Filter LFO rate (Hz)
  filter_lfo_amt?: number;  // Filter LFO amount (Hz)
}

export interface SourceLocation {
  start: number;      // Start byte offset in source
  end: number;        // End byte offset in source
}

export interface SoundEvent {
  start: number;      // Start time in cycles
  end: number;        // End time in cycles
  value: string | number;
  whole_start?: number;
  whole_end?: number;
  meta?: Record<string, string>;  // Effect metadata from Rust
  locations?: SourceLocation[];   // Source locations for highlighting
  tags?: string[];    // Widget tags for filtering (e.g., scope widgets)
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

  // Effect send nodes
  private delayNode: DelayNode | null = null;
  private delayFeedback: GainNode | null = null;
  private delaySend: GainNode | null = null;
  private reverbNode: ConvolverNode | null = null;
  private reverbSend: GainNode | null = null;
  private reverbBuffer: AudioBuffer | null = null;

  // Audio analyser for oscilloscope visualization (global)
  private analyserNode: AnalyserNode | null = null;
  private analyserData: Float32Array<ArrayBuffer> | null = null;

  // Per-scope analysers for filtered visualization
  private scopeAnalysers = new Map<string, {
    analyser: AnalyserNode;
    data: Float32Array<ArrayBuffer>;
  }>();

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
   * Mobile browsers require user interaction to start audio.
   */
  async init(): Promise<void> {
    if (!this.ctx) {
      // Create AudioContext - on iOS/Safari, we may need to use webkitAudioContext
      const AudioContextClass = window.AudioContext || (window as unknown as { webkitAudioContext: typeof AudioContext }).webkitAudioContext;
      this.ctx = new AudioContextClass();
    }

    // Always try to resume - mobile browsers are strict about this
    if (this.ctx.state === 'suspended') {
      try {
        await this.ctx.resume();
      } catch (e) {
        console.warn('Failed to resume AudioContext:', e);
      }
    }

    // Set up master gain if not already done
    if (!this.masterGain) {
      this.masterGain = this.ctx.createGain();
      this.masterGain.gain.value = this.options.gain;

      // Initialize analyser for oscilloscope
      this.initAnalyser();

      // Connect master -> analyser -> destination
      this.masterGain.connect(this.analyserNode!);
      this.analyserNode!.connect(this.ctx.destination);

      // Initialize effects
      this.initDelay();
      this.initReverb();
    }
  }

  private initAnalyser(): void {
    if (!this.ctx) return;

    this.analyserNode = this.ctx.createAnalyser();
    this.analyserNode.fftSize = 2048; // Gives us 1024 data points
    this.analyserNode.smoothingTimeConstant = 0; // No smoothing for crisp waveform
    // Create Float32Array with explicit ArrayBuffer for type compatibility
    const bufferSize = this.analyserNode.frequencyBinCount;
    const buffer = new ArrayBuffer(bufferSize * 4); // 4 bytes per float32
    this.analyserData = new Float32Array(buffer);
  }

  /**
   * Get time-domain audio data for oscilloscope visualization.
   * Returns a Float32Array with values from -1 to 1.
   */
  getTimeDomainData(): Float32Array<ArrayBuffer> | null {
    if (!this.analyserNode || !this.analyserData) return null;
    this.analyserNode.getFloatTimeDomainData(this.analyserData);
    return this.analyserData;
  }

  /**
   * Get the analyser node for direct access.
   */
  get analyser(): AnalyserNode | null {
    return this.analyserNode;
  }

  /**
   * Get or create an analyser for a specific scope widget.
   * The analyser is not connected to anything yet - sounds must connect to it.
   */
  getOrCreateScopeAnalyser(scopeId: string): AnalyserNode {
    if (!this.ctx) throw new Error('AudioContext not initialized');

    let entry = this.scopeAnalysers.get(scopeId);
    if (!entry) {
      const analyser = this.ctx.createAnalyser();
      analyser.fftSize = 2048;
      analyser.smoothingTimeConstant = 0;
      const bufferSize = analyser.frequencyBinCount;
      const buffer = new ArrayBuffer(bufferSize * 4);
      const data = new Float32Array(buffer);
      entry = { analyser, data };
      this.scopeAnalysers.set(scopeId, entry);
    }
    return entry.analyser;
  }

  /**
   * Get time-domain data for a specific scope widget.
   */
  getScopeTimeDomainData(scopeId: string): Float32Array<ArrayBuffer> | null {
    const entry = this.scopeAnalysers.get(scopeId);
    if (!entry) return null;
    entry.analyser.getFloatTimeDomainData(entry.data);
    return entry.data;
  }

  /**
   * Clear all scope analysers (call when pattern changes).
   */
  clearScopeAnalysers(): void {
    this.scopeAnalysers.clear();
  }

  /**
   * Ensure audio context is running (call on any user interaction).
   */
  async ensureRunning(): Promise<void> {
    if (this.ctx && this.ctx.state === 'suspended') {
      try {
        await this.ctx.resume();
      } catch (e) {
        console.warn('Failed to resume AudioContext:', e);
      }
    }
  }

  private initDelay(): void {
    if (!this.ctx || !this.masterGain) return;

    // Delay line with feedback
    this.delayNode = this.ctx.createDelay(2.0);
    this.delayNode.delayTime.value = 0.375; // Dotted eighth note at 120bpm

    this.delayFeedback = this.ctx.createGain();
    this.delayFeedback.gain.value = 0.4;

    this.delaySend = this.ctx.createGain();
    this.delaySend.gain.value = 0; // Dry by default

    // Delay routing: send -> delay -> feedback loop -> master
    this.delaySend.connect(this.delayNode);
    this.delayNode.connect(this.delayFeedback);
    this.delayFeedback.connect(this.delayNode);
    this.delayNode.connect(this.masterGain);
  }

  private initReverb(): void {
    if (!this.ctx || !this.masterGain) return;

    // Create reverb convolver with impulse response
    this.reverbNode = this.ctx.createConvolver();
    this.reverbSend = this.ctx.createGain();
    this.reverbSend.gain.value = 0; // Dry by default

    // Generate synthetic impulse response
    this.reverbBuffer = this.createReverbImpulse(2.0, 2.0);
    this.reverbNode.buffer = this.reverbBuffer;

    // Reverb routing
    this.reverbSend.connect(this.reverbNode);
    this.reverbNode.connect(this.masterGain);
  }

  private createReverbImpulse(duration: number, decay: number): AudioBuffer {
    const sampleRate = this.ctx!.sampleRate;
    const length = sampleRate * duration;
    const buffer = this.ctx!.createBuffer(2, length, sampleRate);

    for (let channel = 0; channel < 2; channel++) {
      const data = buffer.getChannelData(channel);
      for (let i = 0; i < length; i++) {
        // Exponentially decaying noise
        data[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
      }
    }
    return buffer;
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
   * Play a sound at a specific time with optional effects.
   * Tags are used to route audio to scope analysers for filtered visualization.
   */
  playAt(time: number, duration: number, value: string | number, meta?: Record<string, string>, tags?: string[]): void {
    if (!this.ctx || !this.masterGain) return;

    const { waveform, freq, isKick } = this.parseValue(value);
    const effects = this.parseEffects(meta);

    // Find scope tags for routing audio to scope analysers
    const scopeTags = tags?.filter(t => t.startsWith('widget_scope_')) ?? [];

    // Debug: log tags once per distinct set
    if (tags && tags.length > 0) {
      console.log(`[audio] playAt tags=${JSON.stringify(tags)}, scopeTags=${JSON.stringify(scopeTags)}`);
    }

    if (waveform === 'noise') {
      this.playNoise(time, duration, effects, scopeTags);
    } else if (isKick) {
      this.playKick(time, duration, effects, scopeTags);
    } else {
      this.playTone(time, duration, freq, waveform, effects, scopeTags);
    }
  }

  /**
   * Play immediately.
   */
  playNow(duration: number, value: string | number, meta?: Record<string, string>): void {
    this.playAt(this.currentTime, duration, value, meta);
  }

  /**
   * Parse effect metadata from string values to numbers.
   */
  private parseEffects(meta?: Record<string, string>): EffectMeta {
    if (!meta) return {};

    return {
      delay: meta.delay ? parseFloat(meta.delay) : undefined,
      delayfeedback: meta.delayfeedback ? parseFloat(meta.delayfeedback) : undefined,
      room: meta.room ? parseFloat(meta.room) : undefined,
      size: meta.size ? parseFloat(meta.size) : undefined,
      drive: meta.drive ? parseFloat(meta.drive) : undefined,
      gain: meta.gain ? parseFloat(meta.gain) : undefined,
      pan: meta.pan ? parseFloat(meta.pan) : undefined,
      lpf: meta.lpf ? parseFloat(meta.lpf) : undefined,
      hpf: meta.hpf ? parseFloat(meta.hpf) : undefined,
      lpq: meta.lpq ? parseFloat(meta.lpq) : undefined,
      hpq: meta.hpq ? parseFloat(meta.hpq) : undefined,
      comp_threshold: meta.comp_threshold ? parseFloat(meta.comp_threshold) : undefined,
      comp_ratio: meta.comp_ratio ? parseFloat(meta.comp_ratio) : undefined,
      comp_attack: meta.comp_attack ? parseFloat(meta.comp_attack) : undefined,
      comp_release: meta.comp_release ? parseFloat(meta.comp_release) : undefined,
    };
  }

  /**
   * Create a waveshaper curve for saturation/drive.
   */
  private createSaturationCurve(amount: number): Float32Array<ArrayBuffer> {
    const samples = 44100;
    const buffer = new ArrayBuffer(samples * 4);
    const curve = new Float32Array(buffer);
    const k = amount * 50; // Scale factor

    for (let i = 0; i < samples; i++) {
      const x = (i * 2) / samples - 1;
      // Soft clipping curve
      curve[i] = (Math.PI + k) * x / (Math.PI + k * Math.abs(x));
    }
    return curve;
  }

  /**
   * Build the effect chain for a sound and return the output node.
   * Creates per-event effect sends based on metadata.
   * Optionally routes audio to scope analysers for filtered visualization.
   */
  private buildEffectChain(sourceGain: GainNode, time: number, effects: EffectMeta, scopeTags: string[] = []): AudioNode {
    if (!this.ctx || !this.masterGain) return sourceGain;

    let currentNode: AudioNode = sourceGain;

    // Apply per-event gain
    if (effects.gain !== undefined) {
      const gainNode = this.ctx.createGain();
      gainNode.gain.setValueAtTime(effects.gain, time);
      currentNode.connect(gainNode);
      currentNode = gainNode;
    }

    // Apply saturation/drive
    if (effects.drive !== undefined && effects.drive > 0) {
      const waveshaper = this.ctx.createWaveShaper();
      waveshaper.curve = this.createSaturationCurve(effects.drive);
      waveshaper.oversample = '2x';
      currentNode.connect(waveshaper);
      currentNode = waveshaper;
    }

    // Apply low-pass filter
    if (effects.lpf !== undefined && effects.lpf > 0) {
      const lpfNode = this.ctx.createBiquadFilter();
      lpfNode.type = 'lowpass';
      lpfNode.frequency.setValueAtTime(effects.lpf, time);
      // Q ranges from 0.001 to 30, default 1. Higher = more resonance
      lpfNode.Q.value = effects.lpq !== undefined ? Math.max(0.001, Math.min(30, effects.lpq)) : 1;
      currentNode.connect(lpfNode);
      currentNode = lpfNode;
    }

    // Apply high-pass filter
    if (effects.hpf !== undefined && effects.hpf > 0) {
      const hpfNode = this.ctx.createBiquadFilter();
      hpfNode.type = 'highpass';
      hpfNode.frequency.setValueAtTime(effects.hpf, time);
      // Q ranges from 0.001 to 30, default 1. Higher = more resonance
      hpfNode.Q.value = effects.hpq !== undefined ? Math.max(0.001, Math.min(30, effects.hpq)) : 1;
      currentNode.connect(hpfNode);
      currentNode = hpfNode;
    }

    // Apply compressor
    if (effects.comp_threshold !== undefined) {
      const compressor = this.ctx.createDynamicsCompressor();
      // Threshold in dB (-100 to 0)
      compressor.threshold.setValueAtTime(
        Math.max(-100, Math.min(0, effects.comp_threshold)),
        time
      );
      // Ratio (1 to 20, default 12)
      compressor.ratio.setValueAtTime(
        effects.comp_ratio !== undefined ? Math.max(1, Math.min(20, effects.comp_ratio)) : 12,
        time
      );
      // Attack time in seconds (0 to 1, default 0.003)
      compressor.attack.setValueAtTime(
        effects.comp_attack !== undefined ? Math.max(0, Math.min(1, effects.comp_attack)) : 0.003,
        time
      );
      // Release time in seconds (0 to 1, default 0.25)
      compressor.release.setValueAtTime(
        effects.comp_release !== undefined ? Math.max(0, Math.min(1, effects.comp_release)) : 0.25,
        time
      );
      // Knee in dB (softness of compression curve)
      compressor.knee.setValueAtTime(6, time);
      currentNode.connect(compressor);
      currentNode = compressor;
    }

    // Apply panning
    if (effects.pan !== undefined) {
      const panner = this.ctx.createStereoPanner();
      panner.pan.setValueAtTime(Math.max(-1, Math.min(1, effects.pan)), time);
      currentNode.connect(panner);
      currentNode = panner;
    }

    // Connect to master
    currentNode.connect(this.masterGain);

    // Send to delay effect
    if (effects.delay !== undefined && effects.delay > 0 && this.delaySend && this.delayNode) {
      const delaySendGain = this.ctx.createGain();
      delaySendGain.gain.setValueAtTime(effects.delay, time);
      sourceGain.connect(delaySendGain);
      delaySendGain.connect(this.delayNode);

      // Update delay time if specified
      if (effects.delayfeedback !== undefined && this.delayFeedback) {
        this.delayFeedback.gain.setValueAtTime(effects.delayfeedback, time);
      }
    }

    // Send to reverb effect
    if ((effects.room !== undefined && effects.room > 0) || (effects.size !== undefined && effects.size > 0)) {
      const roomAmount = effects.room ?? effects.size ?? 0;
      if (this.reverbSend && this.reverbNode) {
        const reverbSendGain = this.ctx.createGain();
        reverbSendGain.gain.setValueAtTime(roomAmount, time);
        sourceGain.connect(reverbSendGain);
        reverbSendGain.connect(this.reverbNode);
      }
    }

    // Route to scope analysers for filtered visualization
    for (const scopeTag of scopeTags) {
      const analyser = this.getOrCreateScopeAnalyser(scopeTag);
      // Connect the final output node to the scope analyser
      currentNode.connect(analyser);
    }

    return currentNode;
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

  private playTone(time: number, duration: number, freq: number, waveform: OscillatorType, effects: EffectMeta = {}, scopeTags: string[] = []): void {
    if (!this.ctx || !this.masterGain) return;

    // Override waveform if specified in effects
    const actualWave = this.parseWaveform(effects.wave) ?? waveform;

    // ADSR from effects or defaults
    const attack = effects.attack ?? this.options.attack;
    const decay = effects.decay ?? this.options.decay;
    const sustain = effects.sustain ?? this.options.sustain;
    const release = effects.release ?? this.options.release;

    // Unison voices (1-8)
    const unisonCount = Math.min(8, Math.max(1, effects.unison ?? 1));
    const detuneRange = effects.detune ?? 0; // cents

    // Create oscillators for each unison voice
    const oscillators: OscillatorNode[] = [];
    const voiceGains: GainNode[] = [];
    const voiceMixer = this.ctx.createGain();
    voiceMixer.gain.value = 1 / Math.sqrt(unisonCount); // Normalize volume

    for (let i = 0; i < unisonCount; i++) {
      const osc = this.ctx.createOscillator();
      const voiceGain = this.ctx.createGain();

      osc.type = actualWave;

      // Calculate detune for this voice (spread evenly)
      let detuneCents = 0;
      if (unisonCount > 1) {
        const spread = (i / (unisonCount - 1)) * 2 - 1; // -1 to 1
        detuneCents = spread * detuneRange;
      }
      osc.detune.value = detuneCents;
      osc.frequency.value = freq;

      // FM synthesis: modulate carrier frequency
      if (effects.fm_index && effects.fm_index > 0) {
        const fmIndex = effects.fm_index;
        const fmRatio = effects.fm_ratio ?? 2;
        const fmWave = this.parseWaveform(effects.fm_wave) ?? 'sine';

        const modulator = this.ctx.createOscillator();
        const modGain = this.ctx.createGain();

        modulator.type = fmWave;
        modulator.frequency.value = freq * fmRatio;
        modGain.gain.value = freq * fmIndex; // FM depth in Hz

        modulator.connect(modGain);
        modGain.connect(osc.frequency);
        modulator.start(time);
        modulator.stop(time + duration + 0.2);
      }

      voiceGain.gain.value = 1;
      osc.connect(voiceGain);
      voiceGain.connect(voiceMixer);

      oscillators.push(osc);
      voiceGains.push(voiceGain);
    }

    // Filter setup
    const filter = this.ctx.createBiquadFilter();
    filter.type = 'lowpass';
    const baseCutoff = effects.lpf ?? freq * 4;
    filter.frequency.setValueAtTime(baseCutoff, time);
    filter.Q.value = effects.lpq ?? 1;

    // Filter envelope modulation
    if (effects.filter_env && effects.filter_env !== 0) {
      const filterEnvAmount = effects.filter_env;
      const peakCutoff = Math.max(20, Math.min(20000, baseCutoff + filterEnvAmount));
      // Envelope follows amp envelope shape
      filter.frequency.setValueAtTime(baseCutoff, time);
      filter.frequency.linearRampToValueAtTime(peakCutoff, time + attack);
      filter.frequency.exponentialRampToValueAtTime(
        baseCutoff + (peakCutoff - baseCutoff) * sustain,
        time + attack + decay
      );
    }

    // Filter LFO modulation
    if (effects.filter_lfo_rate && effects.filter_lfo_amt) {
      const lfo = this.ctx.createOscillator();
      const lfoGain = this.ctx.createGain();

      lfo.type = 'sine';
      lfo.frequency.value = effects.filter_lfo_rate;
      lfoGain.gain.value = effects.filter_lfo_amt;

      lfo.connect(lfoGain);
      lfoGain.connect(filter.frequency);
      lfo.start(time);
      lfo.stop(time + duration + 0.2);
    }

    // Amplitude envelope
    const gain = this.ctx.createGain();
    const attackEnd = time + attack;
    const decayEnd = attackEnd + decay;
    const releaseStart = time + duration - release;
    const peakGain = 0.6;

    gain.gain.setValueAtTime(0, time);
    gain.gain.linearRampToValueAtTime(peakGain, attackEnd);
    gain.gain.linearRampToValueAtTime(sustain * peakGain, decayEnd);
    gain.gain.setValueAtTime(sustain * peakGain, Math.max(decayEnd, releaseStart));
    gain.gain.linearRampToValueAtTime(0, time + duration);

    // Connect: voices -> mixer -> filter -> gain -> effects
    voiceMixer.connect(filter);
    filter.connect(gain);

    // Apply effect chain (with scope routing)
    this.buildEffectChain(gain, time, effects, scopeTags);

    // Start and stop all oscillators
    oscillators.forEach(osc => {
      osc.start(time);
      osc.stop(time + duration + 0.2);
    });
  }

  private parseWaveform(wave: string | undefined): OscillatorType | null {
    if (!wave) return null;
    const lower = wave.toLowerCase();
    switch (lower) {
      case 'sine': case 'sin': return 'sine';
      case 'saw': case 'sawtooth': return 'sawtooth';
      case 'square': case 'sq': case 'pulse': return 'square';
      case 'tri': case 'triangle': return 'triangle';
      default: return null;
    }
  }

  private playKick(time: number, duration: number, effects: EffectMeta = {}, scopeTags: string[] = []): void {
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

    // Apply effect chain (with scope routing)
    this.buildEffectChain(gain, time, effects, scopeTags);

    osc.start(time);
    osc.stop(time + Math.min(duration, 0.3) + 0.1);
  }

  private playNoise(time: number, duration: number, effects: EffectMeta = {}, scopeTags: string[] = []): void {
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

    // Apply effect chain (with scope routing)
    this.buildEffectChain(gain, time, effects, scopeTags);

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
 *
 * Based on Strudel's cyclist/zyklus architecture:
 * - Uses a clock with lookahead + overlap for robust timing
 * - Schedules events with latency compensation
 * - Warns if events would be scheduled in the past
 */
export type EventCallback = (event: SoundEvent, triggerTime: number, duration: number) => void;

export class PatternScheduler {
  private audio: AudioEngine;
  private running = false;
  private phase = 0;           // Current phase in audio time (seconds)
  private lastTickTime = 0;    // Last time tick() was called
  private timerId: number | null = null;

  // Timing configuration (matching Strudel's defaults)
  public cps = 0.5;            // Cycles per second (tempo)
  public interval = 0.05;      // Clock cycle duration (seconds)
  public lookahead = 0.1;      // How far ahead to look (seconds)
  public latency = 0.1;        // Latency compensation (seconds) - events scheduled this far ahead

  // Query function provided by user
  private queryFn: ((start: number, end: number) => SoundEvent[]) | null = null;

  // Event callback for UI updates (e.g., highlighting)
  public onEvent: EventCallback | null = null;

  // Scheduled event tracking to avoid duplicates
  private scheduledEvents = new Set<string>();

  // Debug: track which values we've logged (to avoid spam)
  private debuggedValues?: Set<string>;

  constructor(audio: AudioEngine) {
    this.audio = audio;
  }

  /**
   * Set the pattern query function.
   */
  setPattern(queryFn: (start: number, end: number) => SoundEvent[]): void {
    this.queryFn = queryFn;
    // Clear scheduled events when pattern changes
    this.scheduledEvents.clear();
  }

  /**
   * Start the scheduler.
   */
  start(): void {
    if (this.running || !this.queryFn) return;

    this.running = true;
    this.phase = 0;
    this.lastTickTime = this.audio.currentTime;
    this.scheduledEvents.clear();

    // Initial tick
    this.tick();
    // Use setInterval for consistent timing
    this.timerId = window.setInterval(this.tick, this.interval * 1000);
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
    this.scheduledEvents.clear();
  }

  /**
   * Check if running.
   */
  get isRunning(): boolean {
    return this.running;
  }

  /**
   * Get the current cycle time (for visualization sync).
   * This accounts for the scheduler's phase tracking and latency compensation.
   */
  getCycleTime(): number {
    if (!this.running) return 0;
    // Calculate current phase by interpolating from last tick
    const currentTime = this.audio.currentTime;
    const elapsed = currentTime - this.lastTickTime;
    const currentPhase = this.phase + elapsed;
    // Subtract latency to match what's actually playing now
    // (audio is scheduled `latency` seconds in the future)
    const compensatedPhase = Math.max(0, currentPhase - this.latency);
    return compensatedPhase * this.cps;
  }

  private tick = (): void => {
    if (!this.running || !this.queryFn) return;

    const currentTime = this.audio.currentTime;
    const elapsed = currentTime - this.lastTickTime;
    this.lastTickTime = currentTime;

    // Advance phase by elapsed time
    this.phase += elapsed;

    // Calculate the lookahead window in audio time
    const lookEnd = this.phase + this.lookahead + this.latency;

    // Convert to cycle time for querying
    const beginCycle = this.phase * this.cps;
    const endCycle = lookEnd * this.cps;

    // Query the pattern for events in this window
    const events = this.queryFn(beginCycle, endCycle);

    // Schedule events
    for (const event of events) {
      // Handle both Map objects (from serde_wasm_bindgen) and plain objects
      const start = event instanceof Map ? event.get('start') : event.start;
      const end = event instanceof Map ? event.get('end') : event.end;
      const value = event instanceof Map ? event.get('value') : event.value;
      const whole_start = event instanceof Map ? event.get('whole_start') : event.whole_start;
      const whole_end = event instanceof Map ? event.get('whole_end') : event.whole_end;

      // Create unique key for this event to avoid double-scheduling
      const eventKey = `${start.toFixed(6)}-${value}`;
      if (this.scheduledEvents.has(eventKey)) {
        continue;
      }

      // Convert cycle time back to audio time
      const onsetAudioTime = start / this.cps;
      const endAudioTime = end / this.cps;

      // Calculate absolute trigger time with latency compensation
      const triggerTime = currentTime - this.phase + onsetAudioTime + this.latency;

      // Warn if we're trying to schedule in the past
      if (triggerTime < currentTime) {
        console.warn(
          `[scheduler] Event scheduled in the past: ${(currentTime - triggerTime).toFixed(3)}s late`
        );
        // Skip events that are too far in the past
        if (triggerTime < currentTime - 0.1) {
          continue;
        }
      }

      const duration = endAudioTime - onsetAudioTime;

      // Use whole duration if available for sustained notes
      const actualDuration = whole_end != null && whole_start != null
        ? (whole_end - whole_start) / this.cps
        : duration;

      // Extract effect metadata
      const meta = event instanceof Map ? event.get('meta') : event.meta;

      // Extract source locations for highlighting
      const locations = event instanceof Map ? event.get('locations') : event.locations;

      // Extract tags for scope filtering
      const tags = event instanceof Map ? event.get('tags') : event.tags;

      // Debug: log raw event to trace tags flow (once per unique value)
      if (!this.debuggedValues?.has(value)) {
        if (!this.debuggedValues) this.debuggedValues = new Set();
        this.debuggedValues.add(value);
        console.log(`[scheduler] event value=${value}, tags=`, tags, 'isMap=', event instanceof Map, 'event keys=', event instanceof Map ? Array.from(event.keys()) : Object.keys(event));
      }

      // Construct normalized event for callback
      const normalizedEvent: SoundEvent = {
        start,
        end,
        value,
        whole_start,
        whole_end,
        meta,
        locations,
        tags,
      };

      this.audio.playAt(triggerTime, actualDuration, value, meta, tags);
      this.scheduledEvents.add(eventKey);

      // Call event callback for UI updates (e.g., highlighting)
      if (this.onEvent) {
        this.onEvent(normalizedEvent, triggerTime, actualDuration);
      }

      // Clean up old event keys (keep set from growing unbounded)
      if (this.scheduledEvents.size > 1000) {
        const keys = Array.from(this.scheduledEvents);
        for (let i = 0; i < 500; i++) {
          this.scheduledEvents.delete(keys[i]);
        }
      }
    }
  };
}
