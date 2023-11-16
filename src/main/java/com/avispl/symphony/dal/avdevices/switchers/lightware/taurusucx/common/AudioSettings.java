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

	CONNECTION_SOURCE("AudioAnalogOutput#ConnectedSource", "api/V1/MEDIA/AUDIO/XP/O3/ConnectedSource"),
	CONNECTION_DESTINATIONS_HDMI3("AudioHDMIInput3#ConnectedDestinations", "api/V1/MEDIA/AUDIO/XP/I3/ConnectedDestinations"),
	CONNECTION_DESTINATIONS_HDMI4("AudioHDMIInput4#ConnectedDestinations", "api/V1/MEDIA/AUDIO/XP/I4/ConnectedDestinations"),
	SIGNAL_PRESENT_HDMI3("AudioHDMIInput3#SignalPresent", "api/V1/MEDIA/AUDIO/I3/SignalPresent"),
	SIGNAL_PRESENT_HDMI4("AudioHDMIInput4#SignalPresent", "api/V1/MEDIA/AUDIO/I4/SignalPresent"),
	SIGNAL_PRESENT_ANALOG("AudioAnalogOutput#SignalPresent", "api/V1/MEDIA/AUDIO/O3/SignalPresent"),
	LOCK_AUDIO_PORT_HDMI1("AudioHDMIInput3#Lock", "api/V1/MEDIA/AUDIO/XP/I3/Lock"),
	LOCK_AUDIO_PORT_HDMI2("AudioHDMIInput4#Lock", "api/V1/MEDIA/AUDIO/XP/I4/Lock"),
	LOCK_AUDIO_PORT_ANALOG("AudioAnalogOutput#Lock", "api/V1/MEDIA/AUDIO/XP/O3/Lock"),
	MUTE_AUDIO_PORT_HDMI1("AudioHDMIInput3#Mute", "api/V1/MEDIA/AUDIO/XP/I3/Mute"),
	MUTE_AUDIO_PORT_HDMI2("AudioHDMIInput4#Mute", "api/V1/MEDIA/AUDIO/XP/I4/Mute"),
	MUTE_AUDIO_PORT_ANALOG("AudioAnalogOutput#Mute", "api/V1/MEDIA/AUDIO/XP/O3/Mute"),
	VOLUME("AudioAnalogOutput#Volume(dB)", "api/V1/MEDIA/AUDIO/O3/VolumedB"),
	BALANCE("AudioAnalogOutput#Balance", "api/V1/MEDIA/AUDIO/O3/Balance"),
	VOLUME_PERCENT("AudioAnalogOutput#Volume(%)", "api/V1/MEDIA/AUDIO/O3/VolumePercent"),
	CONNECTED_INPUT_HDMI1("AudioHDMIInput3#Connected", "api/v1/media/audio/I3/connected"),
	CONNECTED_INPUT_HDMI2("AudioHDMIInput4#Connected", "api/v1/media/audio/I4/connected"),
	SWITCHABLE_OUTPUT1_HDMI3("AudioAnalogOutput#Input3SwitchableState", "api/V1/MEDIA/AUDIO/XP/O3/SWITCHABLE/I3"),
	SWITCHABLE_OUTPUT1_HDMI4("AudioAnalogOutput#Input4SwitchableState", "api/V1/MEDIA/AUDIO/XP/O3/SWITCHABLE/I4"),
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