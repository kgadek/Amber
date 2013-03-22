package amber.client;

import amber.proto.AmberProto.DriverHdr;
import amber.proto.AmberProto.DriverMsg;

import com.google.protobuf.ExtensionRegistry;

public abstract class AmberProxy {

	protected final AmberClient amberClient;
	protected final int deviceType;
	protected final int deviceID;
	
	public AmberProxy(int deviceType, int deviceID, AmberClient amberClient) {
		this.deviceType = deviceType;
		this.deviceID = deviceID;
		this.amberClient = amberClient;
		
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
	
}
