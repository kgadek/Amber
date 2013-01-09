package roboclaw.client;

import java.io.IOException;
import java.util.logging.Logger;

import roboclaw.proto.RoboclawProto;
import amber.client.AmberClient;
import amber.client.AmberProxy;
import amber.proto.AmberProto;
import amber.proto.AmberProto.DriverHdr;
import amber.proto.AmberProto.DriverMsg;

import com.google.protobuf.ExtensionRegistry;


public class RoboclawProxy extends AmberProxy {

	//TODO: enum z typami urządzeń... 
	private final static int DEVICE_TYPE = 2;
	
	private int synNum = 100;	
	private final ExtensionRegistry extensionRegistry;
	
	public RoboclawProxy(AmberClient amberClient, int deviceID) {
		super(DEVICE_TYPE, deviceID, amberClient, Logger.getLogger("RoboclawProxy"));
		
		extensionRegistry = ExtensionRegistry.newInstance();
		RoboclawProto.registerAllExtensions(extensionRegistry);	
	}
	
	synchronized private int getNextSynNum() {
		return synNum++;
	}
	

	@Override
	public ExtensionRegistry getExtensionRegistry() {
		return extensionRegistry;
	}

	@Override
	public void handleDataMsg(DriverHdr header, DriverMsg message) {
		
		
	}
	
	private RoboclawProto.MotorsCommand buildMotorsCommand(MotorsCommand rc) {
		
		if (rc == null) {
			return null;
		}
		
		
		if (rc.m1Speed == null || rc.m2Speed == null) {
			return null;
		}

		RoboclawProto.MotorsCommand.Builder commandBuilder = RoboclawProto.MotorsCommand.newBuilder();

		commandBuilder.setAddress(rc.address);
		
		commandBuilder.setM1Speed(rc.m1Speed);
		
		if (rc.m1Accel != null) {
			commandBuilder.setM1Accel(rc.m1Accel);
		}
		
		if (rc.m1Distance != null) {
			commandBuilder.setM1Distance(rc.m1Distance);
		}
		
		if (rc.m1Buffered != null) {
			commandBuilder.setM1Buffered(rc.m1Buffered);
		}
		
		commandBuilder.setM2Speed(rc.m2Speed);
		
		if (rc.m2Accel != null) {
			commandBuilder.setM2Accel(rc.m2Accel);
		}
		
		if (rc.m2Distance != null) {
			commandBuilder.setM2Distance(rc.m2Distance);
		}
		
		if (rc.m2Buffered != null) {
			commandBuilder.setM2Buffered(rc.m2Buffered);
		}

		return commandBuilder.build();
		
	}
	
	public void sendMotorsEncoderCommand(MotorsCommand rc1, MotorsCommand rc2) {

		DriverMsg.Builder driverMsgBuilder = DriverMsg.newBuilder();
		driverMsgBuilder.setType(AmberProto.DriverMsg.MsgType.DATA);
		
		RoboclawProto.MotorsCommand motorsCommand;
		
		motorsCommand = buildMotorsCommand(rc1);
		if (motorsCommand != null) {
			driverMsgBuilder.addExtension(RoboclawProto.motorsCommands, motorsCommand);
		}
		
		motorsCommand = buildMotorsCommand(rc2);
		if (motorsCommand != null) {
			driverMsgBuilder.addExtension(RoboclawProto.motorsCommands, motorsCommand);
		}
		
		driverMsgBuilder.setSynNum(getNextSynNum());
		
		try {
			amberClient.sendMessage(buildHeader(), driverMsgBuilder.build());
		} catch (IOException e) {
			e.printStackTrace();
		}		
	}

}
