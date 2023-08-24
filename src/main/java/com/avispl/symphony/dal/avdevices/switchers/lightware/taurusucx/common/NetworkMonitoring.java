/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

import java.util.Map;

import com.avispl.symphony.dal.util.StringUtils;

/**
 * NetworkInformation enum provides the network info such as ip, subnetMask, gateway, dhcp, hostname
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/11/2023
 * @since 1.0.0
 */
public enum NetworkMonitoring {
	IP_ADDRESS("IPAddress", "/api/V1/MANAGEMENT/NETWORK/Staticip"),
	SUBNET_MASK("SubnetMask", "/api/V1/MANAGEMENT/NETWORK/StaticNetworkMask"),
	GATEWAY("Gateway", "/api/V1/MANAGEMENT/NETWORK/StaticGatewayAddress"),
	DHCP_STATE("DHCPState", "/api/V1/MANAGEMENT/NETWORK/DhcpEnable"),
	HOSTNAME("HostName", "/api/V1/MANAGEMENT/NETWORK/HostName"),
	;

	/**
	 * Constructor instance
	 *
	 * @param name of {@link #name}
	 * @param request of {@link #request}
	 */
	NetworkMonitoring(String name, String request) {
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
	 * Populates statistics map with values from the cache for NetworkInformation.
	 *
	 * @param stats The map to populate with statistics
	 * @param cache The cache containing the values
	 */
	public static void populateLocalStatistics(Map<String, String> stats, Map<String, String> cache) {
		for (NetworkMonitoring networkInformation : NetworkMonitoring.values()) {
			String key = networkInformation.getName();
			String value = StringUtils.isNullOrEmpty(cache.get(key)) ? LightwareConstant.NONE : cache.get(key);
			switch (networkInformation) {
				case DHCP_STATE:
					if (LightwareConstant.TRUE.equalsIgnoreCase(value)) {
						value = LightwareConstant.ENABLE;
					}
					if (LightwareConstant.FALSE.equalsIgnoreCase(value)) {
						value = LightwareConstant.DISABLE;
					}
					stats.put(key, value);
					break;
				default:
					char firstChar = Character.toUpperCase(value.charAt(0));
					stats.put(key, firstChar + value.substring(1));
					break;
			}
		}
	}
}