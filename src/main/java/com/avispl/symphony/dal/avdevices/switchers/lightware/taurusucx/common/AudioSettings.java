/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

/**
 * AudioSettings Querying the Connected Source, Querying the Connected Destinations, Locking/Unlocking the Audio Port
 * Muting/Unmuting the Audio Port 1, Muting/Unmuting the Audio Port 2, Querying the Audio Signal Presence,
 * Analog Audio Output Volume (db), Setting Analog Audio, Output Volume Percent Setting
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/14/2023
 * @since 1.0.0
 */
public enum AudioSettings {

	CONNECTION_SOURCE("ConnectedSource", "/api/V1/MEDIA/AUDIO/XP/O3/ConnectedSource"),
	CONNECTION_DESTINATIONS_HDMI3("AudioHDMIInput3#ConnectedDestinations", "/api/V1/MEDIA/AUDIO/XP/I3/ConnectedDestinations"),
	CONNECTION_DESTINATIONS_HDMI4("AudioHDMIInput4#ConnectedDestinations", "/api/V1/MEDIA/AUDIO/XP/I4/ConnectedDestinations"),
	SIGNAL_PRESENT_HDMI3("AudioHDMIInput3#SignalPresent", "/api/V1/MEDIA/AUDIO/I3/SignalPresent"),
	SIGNAL_PRESENT_HDMI4("AudioHDMIInput4#SignalPresent", "/api/V1/MEDIA/AUDIO/I4/SignalPresent"),
	SIGNAL_PRESENT_ANALOG("AudioAnalogOutput#SignalPresent", "/api/V1/MEDIA/AUDIO/O3/SignalPresent"),
	LOCK_AUDIO_PORT_HDMI1("AudioHDMIInput3#LockAudioPort", "/api/V1/MEDIA/AUDIO/XP/I3/Lock"),
	LOCK_AUDIO_PORT_HDMI2("AudioHDMIInput4#LockAudioPort", "/api/V1/MEDIA/AUDIO/XP/I4/Lock"),
	LOCK_AUDIO_PORT_ANALOG("AudioAnalogOutput#LockAudioPort", "/api/V1/MEDIA/AUDIO/XP/O3/Lock"),
	MUTE_AUDIO_PORT_HDMI1("AudioHDMIInput3#MuteAudioPort1", "/api/V1/MEDIA/AUDIO/XP/I3/Mute"),
	MUTE_AUDIO_PORT_HDMI2("AudioHDMIInput4#MuteAudioPort1", "/api/V1/MEDIA/AUDIO/XP/I4/Mute"),
	MUTE_AUDIO_PORT_ANALOG("AudioAnalogOutput#MuteAudioPort1", "/api/V1/MEDIA/AUDIO/XP/O3/Mute"),
	MUTE_AUDIO_PORT2_HDMI1("AudioHDMIInput3#MuteAudioPort2", "/api/V1/MEDIA/AUDIO/I3/Mute"),
	MUTE_AUDIO_PORT2_HDMI2("AudioHDMIInput4#MuteAudioPort2", "/api/V1/MEDIA/AUDIO/I4/Mute"),
	MUTE_AUDIO_PORT2_ANALOG("AudioAnalogOutput#MuteAudioPort2", "/api/V1/MEDIA/AUDIO/O3/Mute"),
	VOLUME("AudioAnalogOutput#Volume(dB)", "/api/V1/MEDIA/AUDIO/O3/VolumedB"),
	VOLUME_PERCENT("AudioAnalogOutput#VolumePercent(%)", "/api/V1/MEDIA/AUDIO/O3/VolumePercent"),
	;

	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	AudioSettings(String name, String request) {
		this.name = name;
		this.request = request;
	}

	private final String name;
	private final String request;

	/**
	 * Retrieves {@link #name}
	 *
	 * @return value of {@link #name}
	 */
	public String getName() {
		return name;
	}

	/**
	 * Retrieves {@link #request}
	 *
	 * @return value of {@link #request}
	 */
	public String getRequest() {
		return request;
	}
}