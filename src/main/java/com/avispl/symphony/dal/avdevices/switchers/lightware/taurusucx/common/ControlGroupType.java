/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

/**
 * ControlGroupType class provide Monitoring and controlling group such as
 * AudioSettings,VideoSettings, USBPortSettings,NetworkMonitoring, SerialPortConfiguration
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/17/2023
 * @since 1.0.0
 */
public enum ControlGroupType {

	AUDIO_ANALOG(LightwareConstant.AUDIO_ANALOG_OUTPUT, true, AudioSettings.class),
	AUDIO_HDMI_1(LightwareConstant.AUDIO_HDMI_INPUT_3, false, AudioSettings.class),
	AUDIO_HDMI_2(LightwareConstant.AUDIO_HDMI_INPUT_4, false, AudioSettings.class),
	VIDEO_INPUT_HDMI_3(LightwareConstant.VIDEO_HDMI_INPUT_3, true, VideoSettings.class),
	VIDEO_INPUT_HDMI_4(LightwareConstant.VIDEO_HDMI_INPUT_4, false, VideoSettings.class),
	VIDEO_OUTPUT_HDMI_3(LightwareConstant.VIDEO_HDMI_OUTPUT_3, false, VideoSettings.class),
	VIDEO_OUTPUT_HDMI_4(LightwareConstant.VIDEO_HDMI_OUTPUT_4, false, VideoSettings.class),
	NETWORK_SECURITY(LightwareConstant.NETWORK_SECURITY, true, NetworkSecurity.class),
	SYSTEM_SETTINGS(LightwareConstant.SYSTEM_SETTINGS, true, SystemSettings.class),
	USB_HOST_1(LightwareConstant.USB_HOST_1, true, USBPortSettings.class),
	USB_HOST_2(LightwareConstant.USB_HOST_2, false, USBPortSettings.class),
	USB_HOST_3(LightwareConstant.USB_HOST_3, false, USBPortSettings.class),
	USB_HOST_4(LightwareConstant.USB_HOST_4, false, USBPortSettings.class),
	USB_HOST_HUB(LightwareConstant.USB_HUB, false, USBPortSettings.class),
	NETWORK_INFORMATION(LightwareConstant.NETWORK_INFORMATION, true, NetworkMonitoring.class),
	SERIAL_SETTINGS(LightwareConstant.SERIAL_SETTINGS, true, SerialPortConfiguration.class),
	;

	private final String name;
	private final boolean isMonitoringType;
	private final Class<? extends Enum<?>> enumType;

	/**
	 * * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param isMonitoringType of {@link #isMonitoringType}
	 * @param enumType of {@link #enumType}
	 */
	ControlGroupType(String name, boolean isMonitoringType, Class<? extends Enum<?>> enumType) {
		if (name == null || enumType == null || !enumType.isEnum()) {
			throw new IllegalArgumentException("Invalid arguments");
		}
		this.name = name;
		this.isMonitoringType = isMonitoringType;
		this.enumType = enumType;
	}

	/**
	 * Retrieves {@link #name}
	 *
	 * @return value of {@link #name}
	 */
	public String getName() {
		return name;
	}

	/**
	 * Retrieves {@link #isMonitoringType}
	 *
	 * @return value of {@link #isMonitoringType}
	 */
	public boolean isMonitoringType() {
		return isMonitoringType;
	}

	/**
	 * Retrieves {@link #enumType}
	 *
	 * @return value of {@link #enumType}
	 */
	public Class<? extends Enum<?>> getEnumType() {
		return enumType;
	}

	/**
	 * Returns the {@link ControlGroupType} corresponding to the provided property name.
	 *
	 * @param propertyName the name of the property to search for
	 * @return the {@link ControlGroupType} associated with the property name, or {@code null} if not found
	 */
	public static ControlGroupType getControlGroupByName(String propertyName) {
		for (ControlGroupType controlGroupType : ControlGroupType.values()) {
			if (controlGroupType.getName().contains(propertyName)) {
				return controlGroupType;
			}
		}
		return null; // Return null if no matching ControlGroupType is found
	}
}
