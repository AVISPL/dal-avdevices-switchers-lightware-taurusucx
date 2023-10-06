/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx;

import java.net.ConnectException;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.util.CollectionUtils;

import javax.security.auth.login.LoginException;

import com.avispl.symphony.api.dal.control.Controller;
import com.avispl.symphony.api.dal.dto.control.AdvancedControllableProperty;
import com.avispl.symphony.api.dal.dto.control.ControllableProperty;
import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.dto.monitor.Statistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.api.dal.monitor.Monitorable;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.AudioSettings;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.ControlGroupType;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.LightwareConstant;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.NetworkMonitoring;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.NetworkSecurity;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.SerialPortConfiguration;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.SystemSettings;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.USBPortSettings;
import com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common.VideoSettings;
import com.avispl.symphony.dal.communicator.RestCommunicator;
import com.avispl.symphony.dal.util.StringUtils;

/**
 * LightwareUCXCommunicator An implementation of SocketCommunicator to provide communication and interaction with Lightware Taurus device
 *
 * Static Monitored Statistics
 * Network information
 * <ul>
 *   <li> Querying the DHCP State </li>
 *   <li> Querying the IP Address </li>
 *   <li> Querying the Subnet Mask </li>
 *   <li> Querying the Gateway Address </li>
 *   <li> Querying the Hostname </li>
 * </ul>
 * System Settings
 * <ul>
 *   <li> Querying the Firmware Package Version</li>
 *   <li> Control Lock/Unlock</li>
 *   <li> Identifying the Device</li>
 *   <li> Toggling the Dark Mode Setting</li>
 * </ul>
 * Video Port Settings - General
 * <ul>
 * Locking/Unlocking the Video Port</li>
 *   <li> Muting/Unmuting the Video Port</li>
 *   <li> Querying the Connected Source</li>
 *   <li> Querying the Connected Destinations</li>
 *   <li> Querying the Input Switching Capability</li>
 *   <li> Querying the Video Signal Presence</li>
 *   <li> Querying HDCP settings (Input Port)</li>
 *   <li> Querying HDCP Mode</li>
 *   <li> Querying the Embedded Audio Presence</li>
 *   <li> Muting/Unmuting the Embedded Audio</li>
 * </ul>
 * Audio Port Settings
 * <ul>
 *   <li> Querying the Connected Source</li>
 *   <li> Querying the Connected Destinations</li>
 *   <li> Locking/Unlocking the Audio Port</li>
 *   <li> Muting/Unmuting the Audio Port 1</li>
 *   <li> Muting/Unmuting the Audio Port 2</li>
 *   <li> Querying the Audio Signal Presence</li>
 *   <li> Analog Audio Output Volume (db) Setting</li>
 *   <li> Analog Audio Output Volume Percent Setting</li>
 * </ul>
 * USB Port Settings
 * <ul>
 *   <li> Querying the Connected Source</li>
 *   <li> Querying the Connected Destinations</li>
 *   <li> Locking/Unlocking the USB Port</li>
 * </ul>
 * Network Security
 * <ul>
 *   <li> Enabling/Disabling the Ethernet Port</li>
 *   <li> Querying the Network Service Port Number</li>
 *   <li> Enabling/Disabling the Network Service Port</li>
 *   <li> Querying the Username for Authentication</li>
 *   <li> Querying the Authentication State (False/True)</li>
 * </ul>
 * Serial Port Configuration
 * <ul>
 *   <li> Querying the BAUD Rate Settings</li>
 *   <li> Querying the Stop Bits Setting</li>
 *   <li> Querying the Data Bits</li>
 *   <li> Querying the Parity Setting</li>
 * </ul>
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/14/2023
 * @since 1.0.0
 */
public class LightwareUCXCommunicator extends RestCommunicator implements Monitorable, Controller {

	/**
	 * store cache Map<String,String> are Key and value of it
	 */
	private final Map<String, String> cacheMapOfKeyAndValue = new HashMap<>();
	/**
	 * reentrantLock is a reentrant lock used for synchronization.
	 */
	private final ReentrantLock reentrantLock = new ReentrantLock();
	private ExtendedStatistics localExtendedStatistics;
	private String configManagement;
	private boolean isConfigManagement;
	private boolean isEmergencyDelivery;
	private int failMonitor = 0;
	private int noOfRequest = 0;

	/**
	 * Capitalizes the first character of a given string.
	 *
	 * @param value The input string.
	 * @return A new string with the first character capitalized, or the original string if it's null or empty.
	 */
	private static String capitalizeFirstCharacter(String value) {
		if (value == null || value.isEmpty()) {
			return value;
		}

		char firstChar = Character.toUpperCase(value.charAt(0));
		return firstChar + value.substring(1);
	}

	/**
	 * Retrieves {@link #configManagement}
	 *
	 * @return value of {@link #configManagement}
	 */
	public String getConfigManagement() {
		return configManagement;
	}

	/**
	 * Sets {@code configManagement}
	 *
	 * @param configManagement the {@code java.lang.String} field
	 */
	public void setConfigManagement(String configManagement) {
		this.configManagement = configManagement;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<Statistics> getMultipleStatistics() throws Exception {
		reentrantLock.lock();
		try {
			ExtendedStatistics extendedStatistics = new ExtendedStatistics();
			Map<String, String> stats = new HashMap<>();
			Map<String, String> statsControl = new HashMap<>();
			List<AdvancedControllableProperty> advancedControllableProperties = new ArrayList<>();

			convertConfigManagement();
			if (!isEmergencyDelivery) {
				failMonitor = 0;
				for (ControlGroupType controlGroupType : ControlGroupType.values()) {
					if (controlGroupType.isMonitoringType()) {
						retrieveDetailsByEnumType(controlGroupType.getEnumType());
					}
				}
				if (noOfRequest > 0 && noOfRequest == failMonitor) {
					throw new ResourceNotReachableException("Fail all monitoring and controlling data");
				}
				populateNetworkInformation(stats, statsControl, advancedControllableProperties);
				if (isConfigManagement) {
					extendedStatistics.setControllableProperties(advancedControllableProperties);
					stats.putAll(statsControl);
				}
				extendedStatistics.setStatistics(stats);
				localExtendedStatistics = extendedStatistics;
			}
			isEmergencyDelivery = false;
		} finally {
			reentrantLock.unlock();
		}
		return Collections.singletonList(localExtendedStatistics);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 *
	 * Check for available devices before retrieving the value
	 * ping latency information to Symphony
	 */
	@Override
	public int ping() throws Exception {
		if (isInitialized()) {
			long pingResultTotal = 0L;

			for (int i = 0; i < this.getPingAttempts(); i++) {
				long startTime = System.currentTimeMillis();

				try (Socket puSocketConnection = new Socket(this.host, this.getPort())) {
					puSocketConnection.setSoTimeout(this.getPingTimeout());
					if (puSocketConnection.isConnected()) {
						long pingResult = System.currentTimeMillis() - startTime;
						pingResultTotal += pingResult;
						if (this.logger.isTraceEnabled()) {
							this.logger.trace(String.format("PING OK: Attempt #%s to connect to %s on port %s succeeded in %s ms", i + 1, host, this.getPort(), pingResult));
						}
					} else {
						if (this.logger.isDebugEnabled()) {
							this.logger.debug(String.format("PING DISCONNECTED: Connection to %s did not succeed within the timeout period of %sms", host, this.getPingTimeout()));
						}
						return this.getPingTimeout();
					}
				} catch (SocketTimeoutException | ConnectException tex) {
					throw new RuntimeException("Socket connection timed out", tex);
				} catch (UnknownHostException ex) {
					throw new UnknownHostException(String.format("Connection timed out, UNKNOWN host %s", host));
				} catch (Exception e) {
					if (this.logger.isWarnEnabled()) {
						this.logger.warn(String.format("PING TIMEOUT: Connection to %s did not succeed, UNKNOWN ERROR %s: ", host, e.getMessage()));
					}
					return this.getPingTimeout();
				}
			}
			return Math.max(1, Math.toIntExact(pingResultTotal / this.getPingAttempts()));
		} else {
			throw new IllegalStateException("Cannot use device class without calling init() first");
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void controlProperty(ControllableProperty controllableProperty) {
		reentrantLock.lock();
		try {
			if (localExtendedStatistics == null || localExtendedStatistics.getStatistics().isEmpty() || localExtendedStatistics.getControllableProperties().isEmpty()) {
				return;
			}
			isEmergencyDelivery = true;
			Map<String, String> stats = this.localExtendedStatistics.getStatistics();
			List<AdvancedControllableProperty> advancedControllableProperties = this.localExtendedStatistics.getControllableProperties();
			String property = controllableProperty.getProperty();
			String value = String.valueOf(controllableProperty.getValue());
			String[] propertyGroup = property.split(LightwareConstant.HASH);

			ControlGroupType controlGroupType = ControlGroupType.getControlGroupByName(propertyGroup[0]);
			if (controlGroupType == null) {
				throw new ResourceNotReachableException(String.format("Property name doesn't support: %s", property));
			}

			String result = getValueByPropertyName(controlGroupType, value, property);
			controlGroupTypeByPropertyName(controlGroupType.getEnumType(), property, result);
			updateValueForTheControllableProperty(property, value, stats, advancedControllableProperties);
			updateValueWithSliderControl(stats, value, property, advancedControllableProperties);
		} finally {
			reentrantLock.unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void controlProperties(List<ControllableProperty> controllableProperties) {
		if (CollectionUtils.isEmpty(controllableProperties)) {
			throw new IllegalArgumentException("ControllableProperties can not be null or empty");
		}
		for (ControllableProperty p : controllableProperties) {
			try {
				controlProperty(p);
			} catch (Exception e) {
				logger.error(String.format("Error when control property %s", p.getProperty()), e);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void authenticate() {
		//Lightware Taurus doesn't require API token.
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void internalInit() throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("Internal init is called.");
		}
		super.internalInit();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void internalDestroy() {
		if (localExtendedStatistics != null && localExtendedStatistics.getStatistics() != null && localExtendedStatistics.getControllableProperties() != null) {
			localExtendedStatistics.getStatistics().clear();
			localExtendedStatistics.getControllableProperties().clear();
		}
		isEmergencyDelivery = false;
		cacheMapOfKeyAndValue.clear();
		super.internalDestroy();
	}

	/**
	 * Updates the stats map based on the provided value and property for slider control.
	 *
	 * @param stats The map containing statistics to be updated.
	 * @param value The new value to be updated.
	 * @param property The property associated with the value.
	 * @param advancedControllableProperties A list of advanced controllable properties to be populated.
	 */
	private void updateValueWithSliderControl(Map<String, String> stats, String value, String property,
			List<AdvancedControllableProperty> advancedControllableProperties) {
		if (AudioSettings.VOLUME.getName().equals(property)) {
			stats.put(LightwareConstant.AUDIO_CURRENT_VALUE, value);
			retrieveVolume(AudioSettings.VOLUME_PERCENT.getRequest(), AudioSettings.VOLUME_PERCENT.getName(), true);
			updateVolumeControl(stats, advancedControllableProperties, true);
		}
		if (AudioSettings.VOLUME_PERCENT.getName().equals(property)) {
			stats.put(LightwareConstant.AUDIO_PERCENT_CURRENT_VALUE, value);
			retrieveVolume(AudioSettings.VOLUME.getRequest(), AudioSettings.VOLUME.getName(), true);
			updateVolumeControl(stats, advancedControllableProperties, false);
		}
		if (AudioSettings.BALANCE.getName().equals(property)) {
			stats.put(LightwareConstant.BALANCE_CURRENT_VALUE, value);
		}
	}

	/**
	 *
	 */
	private void updateVolumeControl(Map<String, String> stats, List<AdvancedControllableProperty> advancedControllableProperties, boolean isVolumeControl) {
		String property = AudioSettings.VOLUME.getName();
		String key = LightwareConstant.AUDIO_CURRENT_VALUE;
		if (isVolumeControl) {
			key = LightwareConstant.AUDIO_PERCENT_CURRENT_VALUE;
			property = AudioSettings.VOLUME_PERCENT.getName();
		}
		String value = StringUtils.isNullOrEmpty(cacheMapOfKeyAndValue.get(property)) ? LightwareConstant.NONE : cacheMapOfKeyAndValue.get(property);
		stats.put(key, value);
		updateValueForTheControllableProperty(property, value, stats, advancedControllableProperties);
	}

	private void retrieveVolume(String request, String name, boolean isVolumeControl) {
		try {
			String response = doGet(request);
			if (StringUtils.isNullOrEmpty(response) || LightwareConstant.NULL.equalsIgnoreCase(response)) {
				response = LightwareConstant.NONE;
			}
			cacheMapOfKeyAndValue.put(name, response.trim());
		} catch (Exception e) {
			String errorMessage;
			if (isVolumeControl) {
				errorMessage = "Control Volume(%) successfully, But can't update the Volume(dB) value";
			} else {
				errorMessage = "Control Volume(dB) successfully, But can't update the Volume(%) value";
			}
			throw new IllegalArgumentException(errorMessage, e);
		}
	}

	/**
	 * Retrieves a value based on the given ControlGroupType, property, and input value.
	 *
	 * @param enumType The ControlGroupType associated with the value.
	 * @param value The original input value to be processed.
	 * @param property The property name associated with the value.
	 * @return A processed value based on the input, ControlGroupType, and property.
	 */
	private String getValueByPropertyName(ControlGroupType enumType, String value, String property) {
		String result;
		switch (enumType) {
			case AUDIO_ANALOG:
				if (AudioSettings.VOLUME.getName().equals(property) ||
						AudioSettings.VOLUME_PERCENT.getName().equals(property) || AudioSettings.BALANCE.getName().equals(property)) {
					result = value.trim();
					break;
				}
				result = convertStringToLightwareValue(value);
				break;
			case SYSTEM_SETTINGS:
				if (SystemSettings.CONTROL_LOOK.getName().equals(property)) {
					result = value.toLowerCase(Locale.ROOT);
				} else if (SystemSettings.IDENTIFYING.getName().equals(property)) {
					result = LightwareConstant.EMPTY;
				} else {
					result = convertStringToLightwareValue(value);
				}
				break;
			case VIDEO_OUTPUT_HDMI_3:
			case VIDEO_OUTPUT_HDMI_4:
				if (VideoSettings.DHCP_MODE_OUTPUT_HDMI3.getName().equals(property) || VideoSettings.DHCP_MODE_OUTPUT_HDMI4.getName().equals(property)) {
					result = LightwareConstant.AUTO;
					if (LightwareConstant.NUMBER_ONE.equals(value)) {
						result = LightwareConstant.ALWAYS;
					}
				} else {
					result = convertStringToLightwareValue(value);
				}
				break;
			default:
				result = convertStringToLightwareValue(value);
				break;
		}
		return result;
	}

	/**
	 * Processes a control group based on the provided property name, value, and enum type.
	 *
	 * @param enumType the enum type representing the control group
	 * @param property the property name to match within the enum constants
	 * @param value the value to use in the control group operation
	 * @throws ResourceNotReachableException if an error occurs while processing the control group
	 */
	private void controlGroupTypeByPropertyName(Class<? extends Enum<?>> enumType, String property, String value) throws ResourceNotReachableException {
		try {
			for (Object metric : enumType.getEnumConstants()) {
				String key = metric.getClass().getMethod(LightwareConstant.GET_NAME).invoke(metric).toString();
				if (property.equalsIgnoreCase(key)) {
					String request = metric.getClass().getMethod(LightwareConstant.GET_REQUEST).invoke(metric).toString();
					String response = doPost(request, value);
					if (StringUtils.isNullOrEmpty(response) || LightwareConstant.NULL.equalsIgnoreCase(response)) {
						response = LightwareConstant.NONE;
					}
					cacheMapOfKeyAndValue.put(key, response.trim());
					return;
				}
			}
		} catch (Exception e) {
			throw new ResourceNotReachableException(String.format("Error when controlling property name: %s, value: %s", property, value), e);
		}
	}

	/**
	 * Populates network information using the provided stats map and cached key-value data.
	 *
	 * @param stats The map containing statistics.
	 * @param statsControl the stats are list of Statistics for the control properties
	 * @param advancedControllableProperties A list of advanced controllable properties to be populated.
	 */
	private void populateNetworkInformation(Map<String, String> stats, Map<String, String> statsControl, List<AdvancedControllableProperty> advancedControllableProperties) {
		//populate monitoring properties
		NetworkMonitoring.populateLocalStatistics(stats, cacheMapOfKeyAndValue);
		SerialPortConfiguration.populateLocalStatistics(stats, cacheMapOfKeyAndValue);
		populateNetworkSecurity(stats, cacheMapOfKeyAndValue);
		//populate controlling properties
		populateSystemSettings(stats, statsControl, cacheMapOfKeyAndValue, advancedControllableProperties);
		populateUSBPortSettings(stats, statsControl, cacheMapOfKeyAndValue, advancedControllableProperties);
		populateAudioSettings(stats, statsControl, cacheMapOfKeyAndValue, advancedControllableProperties);
		populateVideoSettings(stats, statsControl, cacheMapOfKeyAndValue, advancedControllableProperties);
	}

	/**
	 * Retrieves details based on the provided enum type.
	 *
	 * @param enumType The enum class representing the monitoring and controlling properties.
	 * @param <T> The type of the enum.
	 * @throws LoginException if user config invalid username and password
	 */
	private <T extends Enum<T>> void retrieveDetailsByEnumType(Class<?> enumType) throws LoginException {
		try {
			for (Object metric : enumType.getEnumConstants()) {
				String key = metric.getClass().getMethod(LightwareConstant.GET_NAME).invoke(metric).toString();
				if (SystemSettings.IDENTIFYING.getName().equals(key)) {
					continue;
				}
				String request = metric.getClass().getMethod(LightwareConstant.GET_REQUEST).invoke(metric).toString();
				noOfRequest++;
				String response = doGet(request);
				if (StringUtils.isNullOrEmpty(response) || LightwareConstant.NULL.equalsIgnoreCase(response)) {
					response = LightwareConstant.NONE;
				}
				cacheMapOfKeyAndValue.put(key, response.trim());
			}
		} catch (SocketTimeoutException | ConnectException tex) {
			throw new RuntimeException("Socket connection timed out", tex);
		} catch (LoginException ex) {
			throw new LoginException("Unauthorized with username and password");
		} catch (Exception e) {
			failMonitor = failMonitor + enumType.getEnumConstants().length;
			logger.error(String.format("Error when retrieving %s", enumType.getName()), e);
		}
	}

	/**
	 * Populates network security-related statistics and advanced controllable properties based on provided data.
	 *
	 * @param stats the stats are list of Statistics
	 * @param cache a map containing cached values for network security keys.
	 */
	private void populateNetworkSecurity(Map<String, String> stats, Map<String, String> cache) {
		for (NetworkSecurity networkSecurity : NetworkSecurity.values()) {
			String key = networkSecurity.getName();
			String value = StringUtils.isNullOrEmpty(cache.get(key)) ? LightwareConstant.NONE : cache.get(key);
			switch (networkSecurity) {
				case HTTP_AUTHENTICATION_STATE:
				case HTTPS_AUTHENTICATION_STATE:
				case ENABLE_ETHERNET_PORT1:
				case ENABLE_ETHERNET_PORT2:
				case ENABLE_ETHERNET_PORT3:
				case ENABLE_ETHERNET_PORT4:
				case ENABLE_ETHERNET_PORT5:
				case HTTP_ENABLE_SERVICE_PORT:
				case HTTPS_ENABLE_SERVICE_PORT:
					value = convertStringToString(value);
					stats.put(key, LightwareConstant.NONE.equals(value) ? value : value.concat("d"));
					break;
				default:
					stats.put(key, value);
					break;
			}
		}
	}

	/**
	 * Populates USB port settings-related statistics and advanced controllable properties based on provided data.
	 *
	 * @param stats the stats are list of Statistics
	 * @param statsControl the stats are list of Statistics for the control properties
	 * @param cache a map containing cached values for USB port settings keys.
	 * @param advancedControllableProperties A list of advanced controllable properties to be populated.
	 */
	private void populateUSBPortSettings(Map<String, String> stats, Map<String, String> statsControl, Map<String, String> cache, List<AdvancedControllableProperty> advancedControllableProperties) {
		for (USBPortSettings usbPortSettings : USBPortSettings.values()) {
			String key = usbPortSettings.getName();
			String value = StringUtils.isNullOrEmpty(cache.get(key)) ? LightwareConstant.NONE : cache.get(key);
			switch (usbPortSettings) {
				case CONNECTION_DESTINATIONS_U1:
				case CONNECTION_DESTINATIONS_U2:
				case CONNECTION_DESTINATIONS_U3:
				case CONNECTION_DESTINATIONS_U4:
				case CONNECTION_SOURCE:
					value = capitalizeFirstCharacter(value);
					if (LightwareConstant.NUMBER_ONE.equalsIgnoreCase(value)) {
						value = LightwareConstant.NONE;
					}
					stats.put(key, value);
					break;
				case LOCKED_USB_PORT_U1:
				case LOCKED_USB_PORT_U2:
				case LOCKED_USB_PORT_U3:
				case LOCKED_USB_PORT_U4:
				case LOCKED_USB_HUB:
					int portValue = convertStringToInt(value);
					if (portValue != -1) {
						AdvancedControllableProperty control = createSwitch(key, portValue, LightwareConstant.FALSE, LightwareConstant.TRUE);
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
						continue;
					}
					stats.put(key, LightwareConstant.NONE);
					break;
				default:
					stats.put(key, LightwareConstant.NUMBER_ONE.equalsIgnoreCase(value) ? LightwareConstant.NONE : value);
					break;
			}
		}
	}

	/**
	 * Populates audio settings-related statistics and advanced controllable properties based on provided data.
	 *
	 * @param stats the stats are list of Statistics
	 * @param statsControl the stats are list of Statistics for the control properties
	 * @param cache a map containing cached values for audio settings keys.
	 * @param advancedControllableProperties A list of advanced controllable properties to be populated.
	 */
	private void populateAudioSettings(Map<String, String> stats, Map<String, String> statsControl, Map<String, String> cache, List<AdvancedControllableProperty> advancedControllableProperties) {
		for (AudioSettings audioSettings : AudioSettings.values()) {
			String key = audioSettings.getName();
			String value = StringUtils.isNullOrEmpty(cache.get(key)) ? LightwareConstant.NONE : cache.get(key);
			switch (audioSettings) {

				case CONNECTION_DESTINATIONS_HDMI3:
				case CONNECTION_DESTINATIONS_HDMI4:
				case CONNECTION_SOURCE:
				case CONNECTED_INPUT_HDMI1:
				case CONNECTED_INPUT_HDMI2:
					value = capitalizeFirstCharacter(value);
					if (LightwareConstant.NUMBER_ONE.equalsIgnoreCase(value)) {
						value = LightwareConstant.NONE;
					}
					stats.put(key, value);
					break;
				case LOCK_AUDIO_PORT_ANALOG:
				case LOCK_AUDIO_PORT_HDMI1:
				case LOCK_AUDIO_PORT_HDMI2:
				case MUTE_AUDIO_PORT_ANALOG:
				case MUTE_AUDIO_PORT_HDMI1:
				case MUTE_AUDIO_PORT_HDMI2:
					int portValue = convertStringToInt(value);
					if (portValue != -1) {
						AdvancedControllableProperty control = createSwitch(key, portValue, LightwareConstant.FALSE, LightwareConstant.TRUE);
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
						continue;
					}
					stats.put(key, LightwareConstant.NONE);
					break;
				case VOLUME:
					stats.put(key, value);
					if (!LightwareConstant.NONE.equals(value)) {
						AdvancedControllableProperty control = createSlider(statsControl, key, String.valueOf(LightwareConstant.MIN_VOLUME), String.valueOf(LightwareConstant.MAX_VOLUME),
								LightwareConstant.MIN_VOLUME, LightwareConstant.MAX_VOLUME, Float.valueOf(value));
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
						statsControl.put(LightwareConstant.AUDIO_CURRENT_VALUE, value);
					}
					break;
				case BALANCE:
					stats.put(key, value);
					if (!LightwareConstant.NONE.equals(value)) {
						AdvancedControllableProperty control = createSlider(statsControl, key, String.valueOf(LightwareConstant.MIN_BALANCE), String.valueOf(LightwareConstant.MAX_BALANCE),
								LightwareConstant.MIN_BALANCE, LightwareConstant.MAX_BALANCE, Float.valueOf(value));
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
						statsControl.put(LightwareConstant.BALANCE_CURRENT_VALUE, value);
					}
					break;
				case VOLUME_PERCENT:
					stats.put(key, value);
					if (!LightwareConstant.NONE.equals(value)) {
						AdvancedControllableProperty controlVolumePercent = createSlider(statsControl, key, String.valueOf(LightwareConstant.MIN_VOLUME_PERCENT),
								String.valueOf(LightwareConstant.MAX_VOLUME_PERCENT),
								LightwareConstant.MIN_VOLUME_PERCENT, LightwareConstant.MAX_VOLUME_PERCENT, Float.valueOf(value));
						addAdvanceControlProperties(advancedControllableProperties, statsControl, controlVolumePercent);
						statsControl.put(LightwareConstant.AUDIO_PERCENT_CURRENT_VALUE, value);
					}
					break;
				default:
					stats.put(key, capitalizeFirstCharacter(value));
					break;
			}
		}
	}

	/**
	 * Populates system settings-related statistics and advanced controllable properties based on provided data.
	 *
	 * @param stats the stats are list of Statistics
	 * @param statsControl the stats are list of Statistics for the control properties
	 * @param cache a map containing cached values for system settings keys.
	 * @param advancedControllableProperties A list of advanced controllable properties to be populated.
	 */
	private void populateSystemSettings(Map<String, String> stats, Map<String, String> statsControl, Map<String, String> cache, List<AdvancedControllableProperty> advancedControllableProperties) {
		for (SystemSettings systemSettings : SystemSettings.values()) {
			String key = systemSettings.getName();
			String value = cache.get(key);
			switch (systemSettings) {
				case ENABLE_DARK_MODE:
					int portValue = convertStringToInt(value);
					if (portValue != -1) {
						AdvancedControllableProperty control = createSwitch(key, portValue, LightwareConstant.FALSE, LightwareConstant.TRUE);
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
						continue;
					}
					stats.put(key, LightwareConstant.NONE);
					break;
				case IDENTIFYING:
					statsControl.put(key, LightwareConstant.EMPTY);
					advancedControllableProperties.add(createButton(key, LightwareConstant.BLINK, LightwareConstant.PROCESSING, 0L));
					break;
				case CONTROL_LOOK:
					String finalValue = value;
					value = LightwareConstant.CONTROL_BLOCK.stream().filter(item -> item.equalsIgnoreCase(finalValue)).findFirst().orElse(LightwareConstant.NONE);
					AdvancedControllableProperty control = createDropdown(key, LightwareConstant.CONTROL_BLOCK.toArray(new String[0]), value);
					addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
					break;
				default:
					stats.put(key, capitalizeFirstCharacter(StringUtils.isNullOrEmpty(value) ? LightwareConstant.NONE : value));
					break;
			}
		}
	}

	/**
	 * Populates video settings-related statistics and advanced controllable properties based on provided data.
	 *
	 * @param stats the stats are list of Statistics
	 * @param statsControl the stats are list of Statistics for the control properties
	 * @param cache a map containing cached values for video settings keys.
	 * @param advancedControllableProperties A list of advanced controllable properties to be populated.
	 */
	private void populateVideoSettings(Map<String, String> stats, Map<String, String> statsControl, Map<String, String> cache, List<AdvancedControllableProperty> advancedControllableProperties) {
		for (VideoSettings videoSettings : VideoSettings.values()) {
			String key = videoSettings.getName();
			String value = StringUtils.isNullOrEmpty(cache.get(key)) ? LightwareConstant.NONE : cache.get(key);
			switch (videoSettings) {
				case CONNECTION_DESTINATIONS_INPUT_HDMI3:
				case CONNECTION_DESTINATIONS_INPUT_HDMI4:
					StringBuilder commaSeparated = extractCommaSeparatedValues(value);
					if (LightwareConstant.NUMBER_ONE.equalsIgnoreCase(value)) {
						commaSeparated = new StringBuilder();
					}
					stats.put(key, commaSeparated.toString().length() > 0 ? commaSeparated.toString() : LightwareConstant.NONE);
					break;
				case CONNECTION_SOURCE_OUTPUT_HDMI4:
				case CONNECTION_SOURCE_OUTPUT_HDMI3:
					value = capitalizeFirstCharacter(value);
					if (LightwareConstant.NUMBER_ONE.equalsIgnoreCase(value)) {
						value = LightwareConstant.NONE;
					}
					stats.put(key, value);
					break;
				case LOCKED_PORT_INPUT_HDMI3:
				case LOCKED_PORT_INPUT_HDMI4:
				case LOCKED_PORT_OUTPUT_HDMI3:
				case LOCKED_PORT_OUTPUT_HDMI4:
				case MUTE_PORT_INPUT_HDMI3:
				case MUTE_PORT_INPUT_HDMI4:
				case MUTE_PORT_OUTPUT_HDMI3:
				case MUTE_PORT_OUTPUT_HDMI4:
				case MUTE_EMBEDDED_PORT_OUTPUT_HDMI3:
				case MUTE_EMBEDDED_PORT_OUTPUT_HDMI4:
					int portValue = convertStringToInt(value);
					if (portValue != -1) {
						AdvancedControllableProperty control = createSwitch(key, portValue, LightwareConstant.FALSE, LightwareConstant.TRUE);
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
						break;
					}
					stats.put(key, LightwareConstant.NONE);
					break;
				case DHCP_MODE_OUTPUT_HDMI3:
				case DHCP_MODE_OUTPUT_HDMI4:
					portValue = -1;
					if (value.equalsIgnoreCase(LightwareConstant.AUTO)) {
						portValue = 0;
					} else if (value.equalsIgnoreCase(LightwareConstant.ALWAYS)) {
						portValue = 1;
					}
					if (portValue != -1) {
						AdvancedControllableProperty control = createSwitch(key, portValue, LightwareConstant.AUTO, LightwareConstant.ALWAYS);
						addAdvanceControlProperties(advancedControllableProperties, statsControl, control);
					} else {
						stats.put(key, LightwareConstant.NONE);
					}
					break;
				default:
					stats.put(key, StringUtils.isNullOrEmpty(value) ? LightwareConstant.NONE : value);
					break;
			}
		}
	}

	/**
	 * Extracts comma-separated values from a given string using a regular expression pattern.
	 *
	 * @param value The input string containing values.
	 * @return a StringBuilder containing comma-separated values.
	 */
	private StringBuilder extractCommaSeparatedValues(String value) {
		Pattern pattern = Pattern.compile(LightwareConstant.REGEX_ARRAY);
		Matcher matcher = pattern.matcher(value);
		StringBuilder commaSeparated = new StringBuilder();
		while (matcher.find()) {
			commaSeparated.append(matcher.group(1)).append(", ");
		}
		if (commaSeparated.length() > 2) {
			commaSeparated.setLength(commaSeparated.length() - 2);
		}
		return commaSeparated;
	}

	/**
	 * Converts a string representation of a boolean value into an integer.
	 *
	 * @param value String representation of a boolean value ("true" or "false").
	 * @return The integer equivalent of the input value (1 for "true", 0 for "false"), or -1 if the value is invalid.
	 */
	private int convertStringToInt(String value) {
		if (LightwareConstant.TRUE.equalsIgnoreCase(value)) {
			return 1;
		}
		if (LightwareConstant.FALSE.equalsIgnoreCase(value)) {
			return 0;
		}
		return -1;
	}

	/**
	 * Converts a given string value to a new string based on constants from the LightwareConstant class.
	 *
	 * @param value The value string to be converted.
	 * @return A new string based on the input value, using the ENABLE, DISABLE, or NONE constants from the LightwareConstant class.
	 */
	private String convertStringToString(String value) {
		if (LightwareConstant.TRUE.equalsIgnoreCase(value)) {
			return LightwareConstant.ENABLE;
		}
		if (LightwareConstant.FALSE.equalsIgnoreCase(value)) {
			return LightwareConstant.DISABLE;
		}
		return LightwareConstant.NONE;
	}

	/**
	 * Converts a given string value to a new string based on constants from the LightwareConstant class.
	 *
	 * @param value The value string to be converted.
	 * @return A new string based on the input value, using the ENABLE, DISABLE, or NONE constants from the LightwareConstant class.
	 */
	private String convertStringToLightwareValue(String value) {
		if (LightwareConstant.NUMBER_ONE.equalsIgnoreCase(value)) {
			return LightwareConstant.TRUE.toLowerCase(Locale.ROOT);
		}
		if (LightwareConstant.ZERO.equalsIgnoreCase(value)) {
			return LightwareConstant.FALSE.toLowerCase(Locale.ROOT);
		}
		return LightwareConstant.NONE;
	}

	/**
	 * This method is used to validate input config management from user
	 */
	private void convertConfigManagement() {
		isConfigManagement = StringUtils.isNotNullOrEmpty(this.configManagement) && this.configManagement.equalsIgnoreCase(LightwareConstant.TRUE);
	}

	/**
	 * Add advancedControllableProperties if advancedControllableProperties different empty
	 *
	 * @param advancedControllableProperties advancedControllableProperties is the list that store all controllable properties
	 * @param stats store all statistics
	 * @param property the property is item advancedControllableProperties
	 * @return String response
	 * @throws IllegalStateException when exception occur
	 */
	private void addAdvanceControlProperties(List<AdvancedControllableProperty> advancedControllableProperties, Map<String, String> stats, AdvancedControllableProperty property) {
		if (property != null) {
			for (AdvancedControllableProperty controllableProperty : advancedControllableProperties) {
				if (controllableProperty.getName().equals(property.getName())) {
					advancedControllableProperties.remove(controllableProperty);
					break;
				}
			}
			stats.put(property.getName(), LightwareConstant.EMPTY);
			advancedControllableProperties.add(property);
		}
	}

	/**
	 * Update the value for the control metric
	 *
	 * @param property is name of the metric
	 * @param value the value is value of properties
	 * @param stats list statistics property
	 * @param advancedControllableProperties the advancedControllableProperties is list AdvancedControllableProperties
	 */
	private void updateValueForTheControllableProperty(String property, String value, Map<String, String> stats, List<AdvancedControllableProperty> advancedControllableProperties) {
		if (!advancedControllableProperties.isEmpty()) {
			for (AdvancedControllableProperty advancedControllableProperty : advancedControllableProperties) {
				if (advancedControllableProperty.getName().equals(property)) {
					stats.remove(property);
					stats.put(property, value);
					advancedControllableProperty.setValue(value);
					break;
				}
			}
		}
	}

	/**
	 * Create switch is control property for metric
	 *
	 * @param name the name of property
	 * @param status initial status (0|1)
	 * @return AdvancedControllableProperty switch instance
	 */
	private AdvancedControllableProperty createSwitch(String name, int status, String labelOff, String labelOn) {
		AdvancedControllableProperty.Switch toggle = new AdvancedControllableProperty.Switch();
		toggle.setLabelOff(labelOff);
		toggle.setLabelOn(labelOn);

		AdvancedControllableProperty advancedControllableProperty = new AdvancedControllableProperty();
		advancedControllableProperty.setName(name);
		advancedControllableProperty.setValue(status);
		advancedControllableProperty.setType(toggle);
		advancedControllableProperty.setTimestamp(new Date());

		return advancedControllableProperty;
	}

	/**
	 * Create a button.
	 *
	 * @param name name of the button
	 * @param label label of the button
	 * @param labelPressed label of the button after pressing it
	 * @param gracePeriod grace period of button
	 * @return This returns the instance of {@link AdvancedControllableProperty} type Button.
	 */
	private AdvancedControllableProperty createButton(String name, String label, String labelPressed, long gracePeriod) {
		AdvancedControllableProperty.Button button = new AdvancedControllableProperty.Button();
		button.setLabel(label);
		button.setLabelPressed(labelPressed);
		button.setGracePeriod(gracePeriod);
		return new AdvancedControllableProperty(name, new Date(), button, LightwareConstant.EMPTY);
	}

	/***
	 * Create dropdown advanced controllable property
	 *
	 * @param name the name of the control
	 * @param initialValue initial value of the control
	 * @return AdvancedControllableProperty dropdown instance
	 */
	private AdvancedControllableProperty createDropdown(String name, String[] values, String initialValue) {
		AdvancedControllableProperty.DropDown dropDown = new AdvancedControllableProperty.DropDown();
		dropDown.setOptions(values);
		dropDown.setLabels(values);

		return new AdvancedControllableProperty(name, new Date(), dropDown, initialValue);
	}

	/***
	 * Create AdvancedControllableProperty slider instance
	 *
	 * @param stats extended statistics
	 * @param name name of the control
	 * @param initialValue initial value of the control
	 * @return AdvancedControllableProperty slider instance
	 */
	private AdvancedControllableProperty createSlider(Map<String, String> stats, String name, String labelStart, String labelEnd, Float rangeStart, Float rangeEnd, Float initialValue) {
		stats.put(name, initialValue.toString());
		AdvancedControllableProperty.Slider slider = new AdvancedControllableProperty.Slider();
		slider.setLabelStart(labelStart);
		slider.setLabelEnd(labelEnd);
		slider.setRangeStart(rangeStart);
		slider.setRangeEnd(rangeEnd);

		return new AdvancedControllableProperty(name, new Date(), slider, initialValue);
	}
}
