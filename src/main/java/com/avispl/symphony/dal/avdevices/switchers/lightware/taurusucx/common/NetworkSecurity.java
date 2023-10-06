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

	HTTP_ENABLE_SERVICE_PORT("NetworkSecurity#ServicePort80Status", "api/V1/MANAGEMENT/NETWORK/SERVICES/HTTP/Enabled"),
	HTTPS_ENABLE_SERVICE_PORT("NetworkSecurity#ServicePort443Status", "api/V1/MANAGEMENT/NETWORK/SERVICES/HTTPS/Enabled"),
	AUTHENTICATION_ROLE("NetworkSecurity#AuthenticationRole", "api/V1/MANAGEMENT/NETWORK/AUTH/USER1/Name"),
	HTTP_AUTHENTICATION_STATE("NetworkSecurity#HTTPAuthenticationStatus", "api/V1/MANAGEMENT/NETWORK/SERVICES/HTTP/AuthenticationEnabled"),
	HTTPS_AUTHENTICATION_STATE("NetworkSecurity#HTTPSAuthenticationStatus", "api/V1/MANAGEMENT/NETWORK/SERVICES/HTTPS/AuthenticationEnabled"),
	ENABLE_ETHERNET_PORT1("NetworkSecurity#EthernetPort1Status", "api/V1/MEDIA/ETHERNET/P1/Enabled"),
	ENABLE_ETHERNET_PORT2("NetworkSecurity#EthernetPort2Status", "api/V1/MEDIA/ETHERNET/P2/Enabled"),
	ENABLE_ETHERNET_PORT3("NetworkSecurity#EthernetPort3Status", "api/V1/MEDIA/ETHERNET/P3/Enabled"),
	ENABLE_ETHERNET_PORT4("NetworkSecurity#EthernetPort4Status", "api/V1/MEDIA/ETHERNET/P4/Enabled"),
	ENABLE_ETHERNET_PORT5("NetworkSecurity#EthernetPort5Status", "api/V1/MEDIA/ETHERNET/P5/Enabled"),

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