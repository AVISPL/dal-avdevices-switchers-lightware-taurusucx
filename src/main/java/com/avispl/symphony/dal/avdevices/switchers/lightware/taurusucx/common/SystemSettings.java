/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

/**
 * SystemSettings Querying the Firmware Package Version, Control Lock/Unlock, Identifying the Device, Toggling the Dark Mode Setting
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/11/2023
 * @since 1.0.0
 */
public enum SystemSettings {

	FIRMWARE_VERSION("SystemSettings#FirmwarePackageVersion", "/api/V1/MANAGEMENT/UID/PACKAGE/Version"),
	IDENTIFYING("SystemSettings#Identify", "/api/V1/MANAGEMENT/UI/identifyMe"),
	ENABLE_DARK_MODE("SystemSettings#EnableDarkMode", "/api/V1/MANAGEMENT/UI/DARKMODE/Enable"),
	CONTROL_LOOK("SystemSettings#ControlLock", "/api/V1/MANAGEMENT/UI/ControlLock"),

	;
	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	SystemSettings(String name, String request) {
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