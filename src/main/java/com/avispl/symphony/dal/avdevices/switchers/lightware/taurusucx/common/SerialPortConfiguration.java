/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

import java.util.Map;

import com.avispl.symphony.dal.util.StringUtils;

/**
 * SerialPortConfiguration class provide BaudRate, StopBits, DataBits, ParitySetting Monitoring
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/11/2023
 * @since 1.0.0
 */
public enum SerialPortConfiguration {

	BAUD_RATE("SerialPort1Configuration#BaudRate(bps)", "api/V1/MEDIA/SERIAL/P1/Baudrate"),
	STOP_BITS("SerialPort1Configuration#StopBits", "api/V1/MEDIA/SERIAL/P1/StopBits"),
	DATA_BITS("SerialPort1Configuration#DataBits", "api/V1/MEDIA/SERIAL/P1/DataBits"),
	PARITY_SETTING("SerialPort1Configuration#Parity", "api/V1/MEDIA/SERIAL/P1/Parity"),
	BAUD_RATE_P1("SerialPort2Configuration#BaudRate(bps)", "api/V1/MEDIA/SERIAL/P2/Baudrate"),
	STOP_BITS_P1("SerialPort2Configuration#StopBits", "api/V1/MEDIA/SERIAL/P2/StopBits"),
	DATA_BITS_P1("SerialPort2Configuration#DataBits", "api/V1/MEDIA/SERIAL/P2/DataBits"),
	PARITY_SETTING_P1("SerialPort2Configuration#Parity", "api/V1/MEDIA/SERIAL/P2/Parity"),
	;

	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	SerialPortConfiguration(String name, String request) {
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

	/**
	 * Populates statistics map with values from the cache for SerialPortConfiguration.
	 *
	 * @param stats The map to populate with statistics
	 * @param cache The cache containing the values
	 */
	public static void populateLocalStatistics(Map<String, String> stats, Map<String, String> cache) {
		for (SerialPortConfiguration serialPortConfiguration : SerialPortConfiguration.values()) {
			String key = serialPortConfiguration.getName();
			String value = StringUtils.isNullOrEmpty(cache.get(key)) ? LightwareConstant.NONE : cache.get(key);
			char firstChar = Character.toUpperCase(value.charAt(0));
			stats.put(key, firstChar + value.substring(1));
		}
	}
}