/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */

/*
 *  Copyright (c) 2023 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.avdevices.switchers.lightware.taurusucx.common;

import java.util.Arrays;
import java.util.List;

/**
 * LightwareConstant class provides constant for monitoring and controlling process
 *
 * @author Kevin / Symphony Dev Team<br>
 * Created on 8/11/2023
 * @since 1.0.0
 */
public class LightwareConstant {

	public static final String NONE = "None";
	public static final String NULL = "Null";
	public static final String TRUE = "True";
	public static final String FALSE = "False";
	public static final String EMPTY = "";
	public static final String ENABLE = "Enable";
	public static final String ENABLED = "Enabled";
	public static final String DISABLE = "Disable";
	public static final String DISABLED = "Disabled";
	public static final String AUTO = "Auto";
	public static final String ALWAYS = "Always";
	public static final String GET_NAME = "getName";
	public static final String GET_REQUEST = "getRequest";
	public static final String BLINK = "Blink";
	public static final String PROCESSING = "Processing";
	public static final String NUMBER_ONE = "1";
	public static final String ZERO = "0";
	public static final String AUDIO_CURRENT_VALUE = "AudioAnalogOutput#VolumeCurrentValue(dB)";
	public static final String BALANCE_CURRENT_VALUE = "AudioAnalogOutput#BalanceCurrentValue";
	public static final String AUDIO_PERCENT_CURRENT_VALUE = "AudioAnalogOutput#VolumeCurrentValue(%)";
	public static final String AUDIO_ANALOG_OUTPUT = "AudioAnalogOutput#";
	public static final String AUDIO_HDMI_INPUT_3 = "AudioHDMIInput3#";
	public static final String VIDEO_HDMI_INPUT_3 = "VideoHDMIInput3#";
	public static final String VIDEO_HDMI_INPUT_4 = "VideoHDMIInput4#";
	public static final String VIDEO_HDMI_OUTPUT_3 = "VideoHDMIOutput1#";
	public static final String VIDEO_HDMI_OUTPUT_4 = "VideoHDMIOutput2#";
	public static final String NETWORK_INFORMATION = "NetworkMonitoring";
	public static final String NETWORK_SECURITY = "NetworkSecurity#";
	public static final String SYSTEM_SETTINGS = "SystemSettings#";
	public static final String SERIAL_SETTINGS = "SerialPortConfiguration#";
	public static final String USB_HOST_1 = "USBHost1#";
	public static final String USB_HOST_2 = "USBHost2#";
	public static final String USB_HOST_3 = "USBHost3#";
	public static final String USB_HOST_4 = "USBHost4#";
	public static final String USB_HUB = "USBHub#";
	public static final String CONNECTION_SOURCE = "ConnectedSource";
	public static final String AUDIO_HDMI_INPUT_4 = "AudioHDMIInput4#";
	public static final String REGEX_ARRAY = "\"(.*?)\"";
	public static final String HASH = "#";
	public static final float MIN_VOLUME = -95.62f;
	public static final float MIN_BALANCE = -100f;
	public static final float MAX_BALANCE = 100f;
	public static final float MAX_VOLUME = 0;
	public static final float MIN_VOLUME_PERCENT = 0;
	public static final float MAX_VOLUME_PERCENT = 100F;
	public static final List<String> CONTROL_BLOCK = Arrays.asList(new String[] { "None", "Locked", "Force locked" });
}
