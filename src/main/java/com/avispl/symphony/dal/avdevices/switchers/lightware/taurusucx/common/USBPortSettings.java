/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

/**
 * USBPortSettings class provides USBConnectionSource, ConnectedDestinations, LockUSBPort
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/11/2023
 * @since 1.0.0
 */
public enum USBPortSettings {
	USB_HOST1("USBHost1#Connected", "api/V1/MEDIA/USB/U1/connected"),
	USB_HOST2("USBHost2#Connected", "api/V1/MEDIA/USB/U2/connected"),
	USB_HOST3("USBHost3#Connected", "api/V1/MEDIA/USB/U3/connected"),
	USB_HOST4("USBHost4#Connected", "api/V1/MEDIA/USB/U4/connected"),
	CONNECTION_DESTINATIONS_U1("USBHost1#ConnectedDestinations", "api/V1/MEDIA/USB/XP/U1/ConnectedDestinations"),
	CONNECTION_DESTINATIONS_U2("USBHost2#ConnectedDestinations", "api/V1/MEDIA/USB/XP/U2/ConnectedDestinations"),
	CONNECTION_DESTINATIONS_U3("USBHost3#ConnectedDestinations", "api/V1/MEDIA/USB/XP/U3/ConnectedDestinations"),
	CONNECTION_DESTINATIONS_U4("USBHost4#ConnectedDestinations", "api/V1/MEDIA/USB/XP/U4/ConnectedDestinations"),
	LOCKED_USB_PORT_U1("USBHost1#Lock", "api/V1/MEDIA/USB/XP/U1/Lock"),
	LOCKED_USB_PORT_U2("USBHost2#Lock", "api/V1/MEDIA/USB/XP/U2/Lock"),
	LOCKED_USB_PORT_U3("USBHost3#Lock", "api/V1/MEDIA/USB/XP/U3/Lock"),
	LOCKED_USB_PORT_U4("USBHost4#Lock", "api/V1/MEDIA/USB/XP/U4/Lock"),
	LOCKED_USB_HUB("USBHub#Lock", "api/V1/MEDIA/USB/XP/H1/Lock"),
	CONNECTION_SOURCE("USBHub#ConnectionSource", "api/V1/MEDIA/USB/XP/H1/ConnectedSource"),
	;

	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	USBPortSettings(String name, String request) {
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