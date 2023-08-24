/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

/**
 * NetworkSecurity class provide the EnableEthernetPort, ServicePortNumber, AuthenticationRole, AuthenticationState
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/11/2023
 * @since 1.0.0
 */
public enum NetworkSecurity {

	ENABLE_ETHERNET_PORT("NetworkSecurity#EnableEthernetPort", "/api/V1/MEDIA/ETHERNET/P4/Enable"),
	PORT_NUMBER("NetworkSecurity#ServicePortNumber", "/api/V1/MANAGEMENT/NETWORK/SERVICES/HTTP/Port"),
	ENABLE_SERVICE_PORT("NetworkSecurity#EnableServicePort", "/api/V1/MANAGEMENT/NETWORK/SERVICES/HTTP/Enable"),
	AUTHENTICATION_ROLE("NetworkSecurity#AuthenticationRole", "/api/V1/MANAGEMENT/NETWORK/AUTH/USER1/Name"),
	AUTHENTICATION_STATE("NetworkSecurity#AuthenticationState ", "/api/V1/MANAGEMENT/NETWORK/SERVICES/HTTP/AuthenticationEnable"),
	;

	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	NetworkSecurity(String name, String request) {
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