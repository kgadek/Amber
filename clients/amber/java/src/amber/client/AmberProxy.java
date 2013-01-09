package amber.client;

import java.io.IOException;
import java.util.logging.Logger;

import amber.proto.AmberProto;
import amber.proto.AmberProto.DriverHdr;
import amber.proto.AmberProto.DriverMsg;

import com.google.protobuf.ExtensionRegistry;

public abstract class AmberProxy {

	protected final AmberClient amberClient;
	protected final int deviceType;
	protected final int deviceID;
	
	protected Logger logger;
	
	public AmberProxy(int deviceType, int deviceID, AmberClient amberClient, Logger logger) {
		this.deviceType = deviceType;
		this.deviceID = deviceID;
		this.amberClient = amberClient;
		this.logger = logger;
		
		amberClient.registerClient(deviceType, deviceID, this);
	}
	
	public abstract void handleDataMsg(DriverHdr header, DriverMsg message);
	public void handlePingMsg(DriverHdr header, DriverMsg message) {};
	public void handlePongMsg(DriverHdr header, DriverMsg message) {};
	public void handleDriverDiedMsg(DriverHdr header, DriverMsg message) {};
	
	public abstract ExtensionRegistry getExtensionRegistry();
	
	protected DriverHdr buildHeader() {
		DriverHdr.Builder driverHdrBuilder = DriverHdr.newBuilder();
		driverHdrBuilder.setDeviceType(deviceType); 
		driverHdrBuilder.setDeviceID(deviceID);
		
		return driverHdrBuilder.build();
	}
	
	public void terminateProxy() {
		logger.info("Sending terminate message");

		DriverMsg.Builder driverMsgBuilder = DriverMsg.newBuilder();
		driverMsgBuilder.setType(AmberProto.DriverMsg.MsgType.CLIENT_DIED);
	
		try {
			amberClient.sendMessage(buildHeader(), driverMsgBuilder.build());
		} catch (IOException e) {
			logger.severe("Error in sending terminate message");
		}
	}
	
}
