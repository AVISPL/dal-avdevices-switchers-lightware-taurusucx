/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx;

import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;

class LightwareUCXCommunicatorTest {

	static LightwareUCXCommunicator packgePDUCommunicator;
	private static final int HTTP_PORT = 8088;
	private static final int HTTPS_PORT = 8443;
	private static final String HOST_NAME = "127.0.0.1";
	private static final String PROTOCOL = "http";

//	@Rule
//	WireMockRule wireMockRule = new WireMockRule(options().port(HTTP_PORT).httpsPort(HTTPS_PORT)
//			.bindAddress(HOST_NAME));

	@BeforeEach
	public void init() throws Exception {
//		wireMockRule.start();
		packgePDUCommunicator = new LightwareUCXCommunicator();
		packgePDUCommunicator.setTrustAllCertificates(false);
		packgePDUCommunicator.setProtocol(PROTOCOL);
//		packgePDUCommunicator.setPort(wireMockRule.port());
		packgePDUCommunicator.setPort(80);
		packgePDUCommunicator.setHost(HOST_NAME);
		packgePDUCommunicator.setContentType("application/json");
		packgePDUCommunicator.setLogin("operator");
		packgePDUCommunicator.setPassword("supervisor");
		packgePDUCommunicator.init();
		packgePDUCommunicator.authenticate();
	}

	@AfterEach
	void stopWireMockRule() {
		packgePDUCommunicator.destroy();
//		wireMockRule.stop();
	}

	/**
	 * Test retrieve network information
	 *
	 * Expect retrieve network successfully
	 */
	@Test
	void testNetworkInformation() {

		ExtendedStatistics extendedStatistics = (ExtendedStatistics)
				packgePDUCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
	}
}