/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

/**
 * VideoSettings class provides the Locking/Unlocking the Video Port, Muting/Unmuting the Video Port,
 * Querying the Connected Source, Querying the Connected Destinations, Querying the Input Switching Capability,
 * Querying the Video Signal Presence, Querying HDCP settings (Input Port), Querying HDCP Mode,
 * Querying the Embedded Audio Presence Muting/Unmuting the Embedded Audio
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/14/2023
 * @since 1.0.0
 */
public enum VideoSettings {

	CONNECTION_SOURCE_OUTPUT_HDMI3("VideoHDMIOutput1#ConnectedSource", "api/V1/MEDIA/VIDEO/XP/O1/ConnectedSource"),
	CONNECTION_SOURCE_OUTPUT_HDMI4("VideoHDMIOutput2#ConnectedSource", "api/V1/MEDIA/VIDEO/XP/O2/ConnectedSource"),
	CONNECTION_DESTINATIONS_INPUT_HDMI3("VideoHDMIInput3#ConnectedDestinations", "api/V1/MEDIA/VIDEO/XP/I3/ConnectedDestinations"),
	CONNECTION_DESTINATIONS_INPUT_HDMI4("VideoHDMIInput4#ConnectedDestinations", "api/V1/MEDIA/VIDEO/XP/I4/ConnectedDestinations"),
	SIGNAL_PRESENT_INPUT_HDMI3("VideoHDMIInput3#SignalPresent", "api/V1/MEDIA/VIDEO/I3/SignalPresent"),
	SIGNAL_PRESENT_INPUT_HDMI4("VideoHDMIInput4#SignalPresent", "api/V1/MEDIA/VIDEO/I4/SignalPresent"),
	SIGNAL_PRESENT_OUTPUT_HDMI3("VideoHDMIOutput1#SignalPresent", "api/V1/MEDIA/VIDEO/I4/SignalPresent"),
	SIGNAL_PRESENT_OUTPUT_HDMI4("VideoHDMIOutput2#SignalPresent", "api/V1/MEDIA/VIDEO/I4/SignalPresent"),
	DHCP_SETTINGS_INPUT_HDMI3("VideoHDMIInput3#HDCPSettings", "api/V1/MEDIA/VIDEO/I3/HDCP/AllowedHdcpVersion"),
	DHCP_SETTINGS_INPUT_HDMI4("VideoHDMIInput4#HDCPSettings", "api/V1/MEDIA/VIDEO/I4/HDCP/AllowedHdcpVersion"),
	DHCP_MODE_OUTPUT_HDMI3("VideoHDMIOutput1#HDCPMode", "api/V1/MEDIA/VIDEO/O1/HDCP/HdcpMode"),
	DHCP_MODE_OUTPUT_HDMI4("VideoHDMIOutput2#HDCPMode", "api/V1/MEDIA/VIDEO/O2/HDCP/HdcpMode"),
	EMBEDDED_AUDIO_PRESENT_INPUT_HDMI3("VideoHDMIInput3#EmbeddedAudioPresent", "api/V1/MEDIA/VIDEO/I3/EmbeddedAudioPresent"),
	EMBEDDED_AUDIO_PRESENT_INPUT_HDMI4("VideoHDMIInput4#EmbeddedAudioPresent", "api/V1/MEDIA/VIDEO/I4/EmbeddedAudioPresent"),
	EMBEDDED_AUDIO_PRESENT_OUTPUT_HDMI3("VideoHDMIOutput1#EmbeddedAudioPresent", "api/V1/MEDIA/VIDEO/O1/EmbeddedAudioPresent"),
	EMBEDDED_AUDIO_PRESENT_OUTPUT_HDMI4("VideoHDMIOutput2#EmbeddedAudioPresent", "api/V1/MEDIA/VIDEO/O2/EmbeddedAudioPresent"),
	LOCKED_PORT_INPUT_HDMI3("VideoHDMIInput3#Lock", "api/V1/MEDIA/VIDEO/XP/I3/Lock"),
	LOCKED_PORT_INPUT_HDMI4("VideoHDMIInput4#Lock", "api/V1/MEDIA/VIDEO/XP/I4/Lock"),
	LOCKED_PORT_OUTPUT_HDMI3("VideoHDMIOutput1#Lock", "api/V1/MEDIA/VIDEO/XP/O1/Lock"),
	LOCKED_PORT_OUTPUT_HDMI4("VideoHDMIOutput2#Lock", "api/V1/MEDIA/VIDEO/XP/O2/Lock"),
	MUTE_PORT_INPUT_HDMI3("VideoHDMIInput3#Mute", "api/V1/MEDIA/VIDEO/XP/I3/Mute"),
	MUTE_PORT_INPUT_HDMI4("VideoHDMIInput4#Mute", "api/V1/MEDIA/VIDEO/XP/I4/Mute"),
	MUTE_PORT_OUTPUT_HDMI3("VideoHDMIOutput1#Mute", "api/V1/MEDIA/VIDEO/XP/O1/Mute"),
	MUTE_PORT_OUTPUT_HDMI4("VideoHDMIOutput2#Mute", "api/V1/MEDIA/VIDEO/XP/O2/Mute"),
	MUTE_EMBEDDED_PORT_OUTPUT_HDMI3("VideoHDMIOutput1#MuteEmbeddedAudio", "api/V1/MEDIA/VIDEO/O1/EmbeddedAudioMute"),
	MUTE_EMBEDDED_PORT_OUTPUT_HDMI4("VideoHDMIOutput2#MuteEmbeddedAudio", "api/V1/MEDIA/VIDEO/O2/EmbeddedAudioMute"),
	CONNECTED_OUTPUT_HDMI1("VideoHDMIOutput1#Connected", "api/v1/media/video/O1/connected"),
	CONNECTED_OUTPUT_HDMI2("VideoHDMIOutput2#Connected", "api/v1/media/video/O2/connected"),
	CONNECTED_INPUT_HDMI3("VideoHDMIInput3#Connected", "api/v1/media/video/I3/connected"),
	CONNECTED_INPUT_HDMI4("VideoHDMIInput4#Connected", "api/v1/media/video/I4/connected"),
	SWITCHABLE_OUTPUT1_HDMI3("VideoHDMIOutput1#Input3SwitchableState", "api/V1/MEDIA/VIDEO/XP/O1/SWITCHABLE/I3"),
	SWITCHABLE_OUTPUT1_HDMI4("VideoHDMIOutput1#Input4SwitchableState", "api/V1/MEDIA/VIDEO/XP/O1/SWITCHABLE/I4"),
	SWITCHABLE_OUTPUT2_HDMI3("VideoHDMIOutput2#Input3SwitchableState", "api/V1/MEDIA/VIDEO/XP/O2/SWITCHABLE/I3"),
	SWITCHABLE_OUTPUT2_HDMI4("VideoHDMIOutput2#Input4SwitchableState", "api/V1/MEDIA/VIDEO/XP/O2/SWITCHABLE/I4"),
	;

	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	VideoSettings(String name, String request) {
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