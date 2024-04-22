/*
 * Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.Map;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.avispl.symphony.api.dal.dto.control.AdvancedControllableProperty;
import com.avispl.symphony.api.dal.dto.control.ControllableProperty;
import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.AudioSettings;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.LightwareConstant;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.NetworkMonitoring;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.SerialPortConfiguration;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.SystemSettings;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.USBPortSettings;

/**
 * LightwareUCXCommunicator
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/14/2023
 * @since 1.0.0
 */
@Tag("Mock")
class LightwareUCXCommunicatorTest {

	static LightwareUCXCommunicator lightwareUCXCommunicator;
	private static final int HTTP_PORT = 8088;
	private static final int HTTPS_PORT = 8443;
	private static final String HOST_NAME = "127.0.0.1";
	private static final String PROTOCOL = "http";

	@Rule
	WireMockRule wireMockRule = new WireMockRule(options().port(HTTP_PORT).httpsPort(HTTPS_PORT)
			.bindAddress(HOST_NAME));

	@BeforeEach
	public void init() throws Exception {
		wireMockRule.start();
		lightwareUCXCommunicator = new LightwareUCXCommunicator();
		lightwareUCXCommunicator.setTrustAllCertificates(false);
		lightwareUCXCommunicator.setProtocol(PROTOCOL);
		lightwareUCXCommunicator.setPort(wireMockRule.port());
		lightwareUCXCommunicator.setHost(HOST_NAME);
		lightwareUCXCommunicator.setContentType("text/plain");
		lightwareUCXCommunicator.init();
		lightwareUCXCommunicator.authenticate();
	}

	@AfterEach
	void stopWireMockRule() throws Exception {
		lightwareUCXCommunicator.destroy();
		wireMockRule.stop();
	}

	/**
	 * Test timeout exception
	 */
	@Test
	void testTCPPingTimeout() throws Exception {
		lightwareUCXCommunicator.destroy();
		wireMockRule.shutdown();
		lightwareUCXCommunicator = new LightwareUCXCommunicator();

		lightwareUCXCommunicator.setTrustAllCertificates(false);
		lightwareUCXCommunicator.setProtocol(PROTOCOL);
		lightwareUCXCommunicator.setPort(80);
		lightwareUCXCommunicator.setHost("127.0.0.1");
		lightwareUCXCommunicator.setPingMode("TCP");
		lightwareUCXCommunicator.setContentType("text/plain");
		lightwareUCXCommunicator.authenticate();
		lightwareUCXCommunicator.init();
		assertThrows(RuntimeException.class, () -> lightwareUCXCommunicator.ping(), "Expect throw RuntimeException with connection timeout");
	}

	@Test
	void testICMPPing() throws Exception {
		lightwareUCXCommunicator.destroy();
		lightwareUCXCommunicator = new LightwareUCXCommunicator();

		lightwareUCXCommunicator.setTrustAllCertificates(false);
		lightwareUCXCommunicator.setProtocol(PROTOCOL);
		lightwareUCXCommunicator.setPort(80);
		lightwareUCXCommunicator.setHost("127.0.0.1");
		lightwareUCXCommunicator.setPingMode("ICMP");
		lightwareUCXCommunicator.authenticate();
		lightwareUCXCommunicator.init();
		assertEquals(1, lightwareUCXCommunicator.ping());
	}

	/**
	 * Test ConfigManagement Configurable with default value
	 */
	@Test
	void testConfigManagementConfigurableWithDefaultValue() throws Exception {
		lightwareUCXCommunicator.ping();
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> advancedControllableProperties = extendedStatistics.getControllableProperties();
		Assert.assertNull(advancedControllableProperties);
	}

	/**
	 * Test ConfigManagement Configurable Is False
	 */
	@Test
	void testConfigManagementConfigurableIsFalse() throws Exception {
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> advancedControllableProperties = extendedStatistics.getControllableProperties();
		Assert.assertNull(advancedControllableProperties);
	}

	/**
	 * Test ConfigManagement Configurable Is True
	 */
	@Test
	void testConfigManagementConfigurableIsTrue() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> advancedControllableProperties = extendedStatistics.getControllableProperties();
		Assert.assertEquals(29, advancedControllableProperties.size());
	}

	/**
	 * Test getMultipleStatistics statistics with network information
	 */
	@Test
	void testGetMultipleStatisticsWithNetworkInformation() throws Exception {
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		Assert.assertEquals("Disabled", stats.get(NetworkMonitoring.DHCP_STATE.getName()));
		Assert.assertEquals("172.31.254.30", stats.get(NetworkMonitoring.IP_ADDRESS.getName()));
		Assert.assertEquals("172.31.254.2", stats.get(NetworkMonitoring.GATEWAY.getName()));
		Assert.assertEquals("Lightware-D4348675", stats.get(NetworkMonitoring.HOSTNAME.getName()));
		Assert.assertEquals("255.255.255.0", stats.get(NetworkMonitoring.SUBNET_MASK.getName()));
		Assert.assertEquals("LW_UCX_4x2_HC30_D", stats.get(NetworkMonitoring.DEVICE_LABEL.getName()));
		Assert.assertEquals("A8:D2:36:01:F7:F1", stats.get(NetworkMonitoring.MAC_ADDRESS.getName()));
		Assert.assertEquals("A8:D2:36:01:F7:F2", stats.get(NetworkMonitoring.MAC_ADDRESS_TYPE1.getName()));
		Assert.assertEquals("A8:D2:36:01:F7:F3", stats.get(NetworkMonitoring.MAC_ADDRESS_TYPE2.getName()));
		Assert.assertEquals("Lightware UCX", stats.get(NetworkMonitoring.MANUFACTURE.getName()));

	}

	/**
	 * Test getMultipleStatistics statistics with SerialPortConfiguration
	 */
	@Test
	void testGetMultipleStatisticsWithSerialPortConfiguration() throws Exception {
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		Assert.assertEquals("19200", stats.get(SerialPortConfiguration.BAUD_RATE.getName()));
		Assert.assertEquals("8", stats.get(SerialPortConfiguration.DATA_BITS.getName()));
		Assert.assertEquals("Odd", stats.get(SerialPortConfiguration.PARITY_SETTING.getName()));
		Assert.assertEquals("1", stats.get(SerialPortConfiguration.STOP_BITS.getName()));
		Assert.assertEquals("19200", stats.get(SerialPortConfiguration.BAUD_RATE_P1.getName()));
		Assert.assertEquals("8", stats.get(SerialPortConfiguration.DATA_BITS_P1.getName()));
		Assert.assertEquals("Odd", stats.get(SerialPortConfiguration.PARITY_SETTING_P1.getName()));
		Assert.assertEquals("1", stats.get(SerialPortConfiguration.STOP_BITS_P1.getName()));
	}

	/**
	 * Test getMultipleStatistics statistics with SystemSettings
	 */
	@Test
	void testGetMultipleStatisticsWithSystemSettings() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();

		// Validate firmware version
		Assert.assertEquals("V2.2.0b4", stats.get(SystemSettings.FIRMWARE_VERSION.getName()));

		// Validate dark mode status
		String darkModeValue = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.ENABLE_DARK_MODE.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", darkModeValue);

		// Validate control look setting
		String controlLookValue = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.CONTROL_LOOK.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("None", controlLookValue);

		// Validate identifying setting
		String identifyingValue = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.IDENTIFYING.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("", identifyingValue);
	}

	/**
	 * Test getMultipleStatistics statistics with USB Host
	 */
	@Test
	void testGetMultipleStatisticsWithUSBHostPort() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		Assert.assertEquals("H1", stats.get(USBPortSettings.CONNECTION_DESTINATIONS_U1.getName()));
		Assert.assertEquals("None", stats.get(USBPortSettings.CONNECTION_DESTINATIONS_U2.getName()));
		Assert.assertEquals("H1", stats.get(USBPortSettings.CONNECTION_DESTINATIONS_U3.getName()));
		Assert.assertEquals("None", stats.get(USBPortSettings.CONNECTION_DESTINATIONS_U4.getName()));
	}


	/**
	 * Test getMultipleStatistics statistics with USB Host
	 */
	@Test
	void testGetMultipleStatisticsWithControlUSB() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String usbPort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_PORT_U1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", usbPort1);
		String usbPort2 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_PORT_U2.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", usbPort2);
		String usbPort3 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_PORT_U3.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", usbPort3);
		String usbPort4 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_PORT_U4.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", usbPort4);
		String usbHub = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_HUB.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", usbHub);
	}

	/**
	 * Test getMultipleStatistics statistics with Audio settings
	 */
	@Test
	void testGetMultipleStatisticsWithAudioSettings() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		Assert.assertEquals("O3", stats.get(AudioSettings.CONNECTION_DESTINATIONS_HDMI3.getName()));
		Assert.assertEquals("False", stats.get(AudioSettings.SIGNAL_PRESENT_HDMI3.getName()));

		Assert.assertEquals("None", stats.get(AudioSettings.CONNECTION_DESTINATIONS_HDMI4.getName()));
		Assert.assertEquals("False", stats.get(AudioSettings.SIGNAL_PRESENT_HDMI4.getName()));

		Assert.assertEquals("I3", stats.get(LightwareConstant.AUDIO_ANALOG_OUTPUT + LightwareConstant.CONNECTION_SOURCE));
		Assert.assertEquals("False", stats.get(AudioSettings.SIGNAL_PRESENT_HDMI4.getName()));
	}

	/**
	 * Test getMultipleStatistics statistics with Audio settings control
	 */
	@Test
	void testGetMultipleStatisticsWithControlLockAudio() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String audioLockPort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.LOCK_AUDIO_PORT_HDMI1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", audioLockPort1);
		String audioLockPort2 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.LOCK_AUDIO_PORT_HDMI2.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", audioLockPort2);
		String audioLockPort3 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.LOCK_AUDIO_PORT_ANALOG.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", audioLockPort3);
	}

	/**
	 * Test getMultipleStatistics statistics with MuteAudioPort1
	 */
	@Test
	void testGetMultipleStatisticsWithControlMuteAudioPort() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String audioMutePort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", audioMutePort1);
		String audioMutePort2 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.MUTE_AUDIO_PORT_HDMI2.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", audioMutePort2);
	}

	/**
	 * Test getMultipleStatistics statistics with Volume value
	 */
	@Test
	void testGetMultipleStatisticsVolumeValue() throws Exception {
		// Enable config management for testing
		lightwareUCXCommunicator.setConfigManagement("true");

		// Retrieve extended statistics and related information
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		Map<String, String> stats = extendedStatistics.getStatistics();
		String audioVolume = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.VOLUME.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("0.0", audioVolume);
		Assert.assertEquals("0.0", stats.get(LightwareConstant.AUDIO_CURRENT_VALUE));
		String audioVolumePercent = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.VOLUME_PERCENT.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("100.0", audioVolumePercent);
		Assert.assertEquals("100.0", stats.get(LightwareConstant.AUDIO_PERCENT_CURRENT_VALUE));
	}

	/**
	 * Test control enable dark mode
	 *
	 * Expect control successfully
	 */
	@Test
	void testControlEnableDarkMode() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String enableDarkMode = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.ENABLE_DARK_MODE.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", enableDarkMode);
		ControllableProperty controlGroupType = new ControllableProperty();
		String key = SystemSettings.ENABLE_DARK_MODE.getName();
		String value = "0";
		controlGroupType.setProperty(key);
		controlGroupType.setValue(value);
		lightwareUCXCommunicator.controlProperty(controlGroupType);

		extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		controllablePropertyList = extendedStatistics.getControllableProperties();
		enableDarkMode = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.ENABLE_DARK_MODE.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("0", enableDarkMode);
	}

	/**
	 * Test control ControlLock
	 *
	 * Expect control successfully
	 */
	@Test
	void testControlControlLock() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String controlLock = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.CONTROL_LOOK.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("None", controlLock);
		ControllableProperty controlGroupType = new ControllableProperty();
		String key = SystemSettings.CONTROL_LOOK.getName();
		String value = "Locked";
		controlGroupType.setProperty(key);
		controlGroupType.setValue(value);
		lightwareUCXCommunicator.controlProperty(controlGroupType);

		extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		controllablePropertyList = extendedStatistics.getControllableProperties();
		controlLock = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.CONTROL_LOOK.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("Locked", controlLock);
	}

	/**
	 * Test control Identify
	 *
	 * Expect control successfully
	 */
	@Test
	void testControlIdentify() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		lightwareUCXCommunicator.getMultipleStatistics().get(0);
		ControllableProperty controlGroupType = new ControllableProperty();
		String key = SystemSettings.IDENTIFYING.getName();
		String value = "";
		controlGroupType.setProperty(key);
		controlGroupType.setValue(value);
		lightwareUCXCommunicator.controlProperty(controlGroupType);

		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String identifying = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(SystemSettings.IDENTIFYING.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("", identifying);
	}

	/**
	 * Test control USB Port Settings LockUSBPort
	 *
	 * Expect LockUSBPort successfully
	 */
	@Test
	void testControlUSBPortSettings() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String lockUSBHub = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_HUB.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", lockUSBHub);
		ControllableProperty controlGroupType = new ControllableProperty();
		String key = USBPortSettings.LOCKED_USB_HUB.getName();
		String value = "0";
		controlGroupType.setProperty(key);
		controlGroupType.setValue(value);
		lightwareUCXCommunicator.controlProperty(controlGroupType);

		extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		controllablePropertyList = extendedStatistics.getControllableProperties();
		lockUSBHub = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(USBPortSettings.LOCKED_USB_HUB.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("0", lockUSBHub);
	}

	/**
	 * Test control Audio Port Settings Mute
	 *
	 * Expect Mute  successfully
	 */
	@Test
	void testControlAudioSettingsMute() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String muteAudioPort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", muteAudioPort1);
		ControllableProperty controlGroupType = new ControllableProperty();
		String key = AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName();
		String value = "0";
		controlGroupType.setProperty(key);
		controlGroupType.setValue(value);
		lightwareUCXCommunicator.controlProperty(controlGroupType);

		extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		controllablePropertyList = extendedStatistics.getControllableProperties();
		muteAudioPort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("0", muteAudioPort1);
	}

	/**
	 * Test control VideoSettings
	 *
	 * Expect Mute  successfully
	 */
	@Test
	void testControlVideoSettings() throws Exception {
		lightwareUCXCommunicator.setConfigManagement("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		List<AdvancedControllableProperty> controllablePropertyList = extendedStatistics.getControllableProperties();
		String muteAudioPort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("1", muteAudioPort1);
		ControllableProperty controlGroupType = new ControllableProperty();
		String key = AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName();
		String value = "0";
		controlGroupType.setProperty(key);
		controlGroupType.setValue(value);
		lightwareUCXCommunicator.controlProperty(controlGroupType);

		extendedStatistics = (ExtendedStatistics) lightwareUCXCommunicator.getMultipleStatistics().get(0);
		controllablePropertyList = extendedStatistics.getControllableProperties();
		muteAudioPort1 = String.valueOf(controllablePropertyList
				.stream()
				.filter(item -> item.getName().equals(AudioSettings.MUTE_AUDIO_PORT_HDMI1.getName()))
				.findFirst()
				.get()
				.getValue());
		Assert.assertEquals("0", muteAudioPort1);
	}
}