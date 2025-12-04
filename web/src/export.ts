/**
 * Audio export utilities for crumble.
 *
 * Provides WAV encoding and offline rendering for exporting patterns.
 */

import type { SoundEvent, EffectMeta } from './audio';

/**
 * Encode an AudioBuffer to WAV format.
 * Returns a Blob containing the WAV file.
 */
export function encodeWAV(buffer: AudioBuffer): Blob {
  const numChannels = buffer.numberOfChannels;
  const sampleRate = buffer.sampleRate;
  const format = 1; // PCM
  const bitsPerSample = 16;

  // Interleave channels
  const length = buffer.length * numChannels * (bitsPerSample / 8);
  const arrayBuffer = new ArrayBuffer(44 + length);
  const view = new DataView(arrayBuffer);

  // WAV header
  writeString(view, 0, 'RIFF');
  view.setUint32(4, 36 + length, true);
  writeString(view, 8, 'WAVE');
  writeString(view, 12, 'fmt ');
  view.setUint32(16, 16, true); // Subchunk1Size
  view.setUint16(20, format, true);
  view.setUint16(22, numChannels, true);
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * numChannels * (bitsPerSample / 8), true);
  view.setUint16(32, numChannels * (bitsPerSample / 8), true);
  view.setUint16(34, bitsPerSample, true);
  writeString(view, 36, 'data');
  view.setUint32(40, length, true);

  // Write interleaved samples
  const channels: Float32Array[] = [];
  for (let i = 0; i < numChannels; i++) {
    channels.push(buffer.getChannelData(i));
  }

  let offset = 44;
  for (let i = 0; i < buffer.length; i++) {
    for (let ch = 0; ch < numChannels; ch++) {
      const sample = Math.max(-1, Math.min(1, channels[ch][i]));
      const intSample = sample < 0 ? sample * 0x8000 : sample * 0x7FFF;
      view.setInt16(offset, intSample, true);
      offset += 2;
    }
  }

  return new Blob([arrayBuffer], { type: 'audio/wav' });
}

function writeString(view: DataView, offset: number, str: string): void {
  for (let i = 0; i < str.length; i++) {
    view.setUint8(offset + i, str.charCodeAt(i));
  }
}

/**
 * Trigger a download of a Blob as a file.
 */
export function downloadBlob(blob: Blob, filename: string): void {
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

/**
 * Format duration in seconds to MM:SS format.
 */
export function formatDuration(seconds: number): string {
  const mins = Math.floor(seconds / 60);
  const secs = Math.floor(seconds % 60);
  return `${mins}:${secs.toString().padStart(2, '0')}`;
}

export interface RenderOptions {
  cycles: number;      // Number of cycles to render
  cps: number;         // Cycles per second (tempo)
  sampleRate?: number; // Sample rate (default 44100)
  gain?: number;       // Master gain (default 0.3)
}

/**
 * Offline renderer for exporting patterns to audio files.
 */
export class OfflineRenderer {
  private options: Required<RenderOptions>;

  constructor(options: RenderOptions) {
    this.options = {
      cycles: options.cycles,
      cps: options.cps,
      sampleRate: options.sampleRate ?? 44100,
      gain: options.gain ?? 0.3,
    };
  }

  /**
   * Render a pattern to an AudioBuffer.
   */
  async render(
    queryFn: (start: number, end: number) => SoundEvent[]
  ): Promise<AudioBuffer> {
    const { cycles, cps, sampleRate, gain } = this.options;
    const duration = cycles / cps;
    const length = Math.ceil(duration * sampleRate);

    // Create offline context
    const ctx = new OfflineAudioContext(2, length, sampleRate);
    const masterGain = ctx.createGain();
    masterGain.gain.value = gain;
    masterGain.connect(ctx.destination);

    // Set up effects
    const { delayNode, reverbNode } = this.createEffects(ctx, masterGain);

    // Query all events for the entire duration
    const events = queryFn(0, cycles);

    // Schedule all events
    for (const event of events) {
      const start = event instanceof Map ? event.get('start') : event.start;
      const end = event instanceof Map ? event.get('end') : event.end;
      const value = event instanceof Map ? event.get('value') : event.value;
      const meta = event instanceof Map ? event.get('meta') : event.meta;

      const onsetTime = start / cps;
      const endTime = end / cps;
      const eventDuration = endTime - onsetTime;

      const effects = this.parseEffects(meta);
      this.scheduleSound(ctx, masterGain, delayNode, reverbNode, onsetTime, eventDuration, value, effects);
    }

    // Render
    return ctx.startRendering();
  }

  private createEffects(
    ctx: OfflineAudioContext,
    masterGain: GainNode
  ): { delayNode: DelayNode; delayFeedback: GainNode; reverbNode: ConvolverNode } {
    // Delay
    const delayNode = ctx.createDelay(2.0);
    delayNode.delayTime.value = 0.375;
    const delayFeedback = ctx.createGain();
    delayFeedback.gain.value = 0.4;
    delayNode.connect(delayFeedback);
    delayFeedback.connect(delayNode);
    delayNode.connect(masterGain);

    // Reverb
    const reverbNode = ctx.createConvolver();
    const irLength = ctx.sampleRate * 2;
    const irBuffer = ctx.createBuffer(2, irLength, ctx.sampleRate);
    for (let ch = 0; ch < 2; ch++) {
      const data = irBuffer.getChannelData(ch);
      for (let i = 0; i < irLength; i++) {
        data[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / irLength, 2);
      }
    }
    reverbNode.buffer = irBuffer;
    reverbNode.connect(masterGain);

    return { delayNode, delayFeedback, reverbNode };
  }

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

  private scheduleSound(
    ctx: OfflineAudioContext,
    masterGain: GainNode,
    delayNode: DelayNode,
    reverbNode: ConvolverNode,
    time: number,
    duration: number,
    value: string | number,
    effects: EffectMeta
  ): void {
    const { waveform, freq, isKick } = this.parseValue(value);

    if (waveform === 'noise') {
      this.playNoise(ctx, masterGain, delayNode, reverbNode, time, duration, effects);
    } else if (isKick) {
      this.playKick(ctx, masterGain, delayNode, reverbNode, time, duration, effects);
    } else {
      this.playTone(ctx, masterGain, delayNode, reverbNode, time, duration, freq, waveform, effects);
    }
  }

  private parseValue(value: string | number): { waveform: OscillatorType | 'noise'; freq: number; isKick: boolean } {
    if (typeof value === 'number') {
      const freq = value < 128 ? 440 * Math.pow(2, (value - 69) / 12) : value;
      return { waveform: 'sine', freq, isKick: false };
    }

    const lower = value.toLowerCase();

    switch (lower) {
      case 'sine': case 'sin':
        return { waveform: 'sine', freq: 440, isKick: false };
      case 'saw': case 'sawtooth':
        return { waveform: 'sawtooth', freq: 440, isKick: false };
      case 'square': case 'sq': case 'pulse':
        return { waveform: 'square', freq: 440, isKick: false };
      case 'tri': case 'triangle':
        return { waveform: 'triangle', freq: 440, isKick: false };
      case 'noise': case 'white':
        return { waveform: 'noise', freq: 0, isKick: false };
    }

    const noteMatch = lower.match(/^([a-g])([#b]?)(\d+)?$/);
    if (noteMatch) {
      const [, note, accidental, octaveStr] = noteMatch;
      const octave = octaveStr ? parseInt(octaveStr) : 4;
      const noteValues: Record<string, number> = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
      let midi = noteValues[note] + (octave + 1) * 12;
      if (accidental === '#') midi += 1;
      if (accidental === 'b') midi -= 1;
      return { waveform: 'sawtooth', freq: 440 * Math.pow(2, (midi - 69) / 12), isKick: false };
    }

    switch (lower) {
      case 'kick': case 'bd':
        return { waveform: 'sine', freq: 150, isKick: true };
      case 'snare': case 'sd': case 'hihat': case 'hh': case 'clap': case 'cp':
        return { waveform: 'noise', freq: 0, isKick: false };
      default:
        return { waveform: 'sine', freq: 440, isKick: false };
    }
  }

  private buildEffectChain(
    ctx: OfflineAudioContext,
    sourceGain: GainNode,
    masterGain: GainNode,
    delayNode: DelayNode,
    reverbNode: ConvolverNode,
    time: number,
    effects: EffectMeta
  ): void {
    let currentNode: AudioNode = sourceGain;

    // Per-event gain
    if (effects.gain !== undefined) {
      const gainNode = ctx.createGain();
      gainNode.gain.setValueAtTime(effects.gain, time);
      currentNode.connect(gainNode);
      currentNode = gainNode;
    }

    // Drive/saturation
    if (effects.drive !== undefined && effects.drive > 0) {
      const waveshaper = ctx.createWaveShaper();
      const samples = 44100;
      const buffer = new ArrayBuffer(samples * 4);
      const curve = new Float32Array(buffer);
      const k = effects.drive * 50;
      for (let i = 0; i < samples; i++) {
        const x = (i * 2) / samples - 1;
        curve[i] = (Math.PI + k) * x / (Math.PI + k * Math.abs(x));
      }
      waveshaper.curve = curve;
      waveshaper.oversample = '2x';
      currentNode.connect(waveshaper);
      currentNode = waveshaper;
    }

    // LPF
    if (effects.lpf !== undefined && effects.lpf > 0) {
      const lpfNode = ctx.createBiquadFilter();
      lpfNode.type = 'lowpass';
      lpfNode.frequency.setValueAtTime(effects.lpf, time);
      lpfNode.Q.value = effects.lpq !== undefined ? Math.max(0.001, Math.min(30, effects.lpq)) : 1;
      currentNode.connect(lpfNode);
      currentNode = lpfNode;
    }

    // HPF
    if (effects.hpf !== undefined && effects.hpf > 0) {
      const hpfNode = ctx.createBiquadFilter();
      hpfNode.type = 'highpass';
      hpfNode.frequency.setValueAtTime(effects.hpf, time);
      hpfNode.Q.value = effects.hpq !== undefined ? Math.max(0.001, Math.min(30, effects.hpq)) : 1;
      currentNode.connect(hpfNode);
      currentNode = hpfNode;
    }

    // Compressor
    if (effects.comp_threshold !== undefined) {
      const compressor = ctx.createDynamicsCompressor();
      compressor.threshold.setValueAtTime(Math.max(-100, Math.min(0, effects.comp_threshold)), time);
      compressor.ratio.setValueAtTime(effects.comp_ratio ?? 12, time);
      compressor.attack.setValueAtTime(effects.comp_attack ?? 0.003, time);
      compressor.release.setValueAtTime(effects.comp_release ?? 0.25, time);
      compressor.knee.setValueAtTime(6, time);
      currentNode.connect(compressor);
      currentNode = compressor;
    }

    // Pan
    if (effects.pan !== undefined) {
      const panner = ctx.createStereoPanner();
      panner.pan.setValueAtTime(Math.max(-1, Math.min(1, effects.pan)), time);
      currentNode.connect(panner);
      currentNode = panner;
    }

    // Connect to master
    currentNode.connect(masterGain);

    // Delay send
    if (effects.delay !== undefined && effects.delay > 0) {
      const delaySendGain = ctx.createGain();
      delaySendGain.gain.setValueAtTime(effects.delay, time);
      sourceGain.connect(delaySendGain);
      delaySendGain.connect(delayNode);
    }

    // Reverb send
    if ((effects.room ?? 0) > 0 || (effects.size ?? 0) > 0) {
      const roomAmount = effects.room ?? effects.size ?? 0;
      const reverbSendGain = ctx.createGain();
      reverbSendGain.gain.setValueAtTime(roomAmount, time);
      sourceGain.connect(reverbSendGain);
      reverbSendGain.connect(reverbNode);
    }
  }

  private playTone(
    ctx: OfflineAudioContext,
    masterGain: GainNode,
    delayNode: DelayNode,
    reverbNode: ConvolverNode,
    time: number,
    duration: number,
    freq: number,
    waveform: OscillatorType,
    effects: EffectMeta
  ): void {
    const osc = ctx.createOscillator();
    const gain = ctx.createGain();
    const filter = ctx.createBiquadFilter();

    osc.type = waveform;
    osc.frequency.value = freq;

    filter.type = 'lowpass';
    filter.frequency.setValueAtTime(freq * 4, time);
    filter.frequency.exponentialRampToValueAtTime(freq * 2, time + 0.1);
    filter.Q.value = 1;

    // ADSR
    const attack = 0.005, decay = 0.1, sustain = 0.3, release = 0.1;
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
    this.buildEffectChain(ctx, gain, masterGain, delayNode, reverbNode, time, effects);

    osc.start(time);
    osc.stop(time + duration + 0.1);
  }

  private playKick(
    ctx: OfflineAudioContext,
    masterGain: GainNode,
    delayNode: DelayNode,
    reverbNode: ConvolverNode,
    time: number,
    duration: number,
    effects: EffectMeta
  ): void {
    const osc = ctx.createOscillator();
    const gain = ctx.createGain();

    osc.type = 'sine';
    osc.frequency.setValueAtTime(150, time);
    osc.frequency.exponentialRampToValueAtTime(50, time + 0.05);

    gain.gain.setValueAtTime(1, time);
    gain.gain.exponentialRampToValueAtTime(0.01, time + Math.min(duration, 0.3));

    osc.connect(gain);
    this.buildEffectChain(ctx, gain, masterGain, delayNode, reverbNode, time, effects);

    osc.start(time);
    osc.stop(time + Math.min(duration, 0.3) + 0.1);
  }

  private playNoise(
    ctx: OfflineAudioContext,
    masterGain: GainNode,
    delayNode: DelayNode,
    reverbNode: ConvolverNode,
    time: number,
    duration: number,
    effects: EffectMeta
  ): void {
    const bufferSize = ctx.sampleRate * duration;
    const buffer = ctx.createBuffer(1, bufferSize, ctx.sampleRate);
    const data = buffer.getChannelData(0);

    for (let i = 0; i < bufferSize; i++) {
      data[i] = Math.random() * 2 - 1;
    }

    const source = ctx.createBufferSource();
    const gain = ctx.createGain();
    const filter = ctx.createBiquadFilter();

    source.buffer = buffer;
    filter.type = 'highpass';
    filter.frequency.value = 1000;

    gain.gain.setValueAtTime(0, time);
    gain.gain.linearRampToValueAtTime(0.5, time + 0.005);
    gain.gain.exponentialRampToValueAtTime(0.01, time + duration);

    source.connect(filter);
    filter.connect(gain);
    this.buildEffectChain(ctx, gain, masterGain, delayNode, reverbNode, time, effects);

    source.start(time);
    source.stop(time + duration + 0.1);
  }
}
