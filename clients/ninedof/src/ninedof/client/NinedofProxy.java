package ninedof.client;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

import ninedof.proto.NinedofProto;
import ninedof.proto.NinedofProto.DataRequest;
import ninedof.proto.NinedofProto.SensorData;
import ninedof.proto.NinedofProto.SubscribeAction;
import amber.client.AmberClient;
import amber.client.AmberConnectionException;
import amber.client.AmberListener;
import amber.client.AmberProxy;
import amber.client.DriverTimeoutException;
import amber.client.FutureObject;
import amber.proto.AmberProto;
import amber.proto.AmberProto.DriverHdr;
import amber.proto.AmberProto.DriverMsg;

import com.google.protobuf.ExtensionRegistry;

public class NinedofProxy extends AmberProxy {

	//TODO: enum z typami urządzeń...
	private final static int DEVICE_TYPE = 1;
	
	private Map<Integer, FutureObject> futureObjectsMap = new ConcurrentHashMap<Integer, FutureObject>();
	private AmberListener<NinedofData> ninedofDataListener;
	private final ReentrantLock listenerLock = new ReentrantLock();
	
	private int synNum = 100;	
	private final ExtensionRegistry extensionRegistry;
	
	public NinedofProxy(AmberClient amberClient, int deviceID) {
		super(DEVICE_TYPE, deviceID, amberClient);
		
		extensionRegistry = ExtensionRegistry.newInstance();
		NinedofProto.registerAllExtensions(extensionRegistry);
	}

	synchronized private int getNextSynNum() {
		return synNum++;
	}


	private void fillStructure(NinedofData ninedofData, DriverMsg message) {
		SensorData sensorData = message.getExtension(NinedofProto.sensorData);
		
		ninedofData.setAccel(new NinedofData.AxesData(sensorData
				.getAccel().getXAxis(), sensorData.getAccel().getYAxis(),
				sensorData.getAccel().getZAxis()));

		ninedofData.setGyro(new NinedofData.AxesData(sensorData
				.getGyro().getXAxis(), sensorData.getGyro().getYAxis(),
				sensorData.getGyro().getZAxis()));

		ninedofData.setMagnet(new NinedofData.AxesData(sensorData
				.getMagnet().getXAxis(), sensorData.getMagnet().getYAxis(),
				sensorData.getMagnet().getZAxis()));
	}

	@Override
	public void handleDataMsg(DriverHdr header, DriverMsg message) {
		
		if (!message.hasAckNum() || message.getAckNum() == 0) {
		
			NinedofData ninedofData = new NinedofData();
			fillStructure(ninedofData, message);
			ninedofData.setAvailable();
			
			synchronized(listenerLock) {
				if (ninedofDataListener != null) {
					ninedofDataListener.handle(ninedofData);
				}
			}
						
		} else {	
			int ackNum = message.getAckNum();
		
			// TODO: automatically removing abandoned futureObjects
			if (futureObjectsMap.containsKey(ackNum)) {
				NinedofData ninedofData = (NinedofData)futureObjectsMap.remove(ackNum);
		
				fillStructure(ninedofData, message);
				ninedofData.setAvailable();
			}
		}
	}
	
	public void registerNinedofDataListener(int freq, boolean accel, boolean gyro, boolean magnet, AmberListener<NinedofData> listener) throws AmberConnectionException {
		DriverMsg driverMsg = buildSubscribeActionMsg(freq, accel, gyro, magnet);
		
		synchronized(listenerLock) {
			ninedofDataListener = listener;
		}
		
		try {
			amberClient.sendMessage(buildHeader(), driverMsg);
		} catch (IOException e) {
			throw new AmberConnectionException();
		}
	}
	
	public void unregisterDataListener() throws AmberConnectionException {
		DriverMsg driverMsg = buildSubscribeActionMsg(0, false, false, false);
		
		synchronized(listenerLock) {
			ninedofDataListener = null;
		}
		
		try {
			amberClient.sendMessage(buildHeader(), driverMsg);
		} catch (IOException e) {
			throw new AmberConnectionException();
		}
	}

	private DriverMsg buildSubscribeActionMsg(int freq, boolean accel, boolean gyro, boolean magnet) {

		SubscribeAction.Builder subscribeActionBuilder = SubscribeAction.newBuilder();
		subscribeActionBuilder.setFreq(freq);
		subscribeActionBuilder.setAccel(accel);
		subscribeActionBuilder.setGyro(gyro);
		subscribeActionBuilder.setMagnet(magnet);

		DriverMsg.Builder driverMsgBuilder = DriverMsg.newBuilder();
		driverMsgBuilder.setType(AmberProto.DriverMsg.MsgType.DATA);
		driverMsgBuilder.setExtension(NinedofProto.subscribeAction,
				subscribeActionBuilder.build());

		return driverMsgBuilder.build();
	}
	
	
	private DriverMsg buildDataRequestMsg(int synNum, boolean accel,
			boolean gyro, boolean magnet) {

		DataRequest.Builder dataRequestBuilder = DataRequest.newBuilder();
		dataRequestBuilder.setAccel(accel);
		dataRequestBuilder.setGyro(gyro);
		dataRequestBuilder.setMagnet(magnet);

		DriverMsg.Builder driverMsgBuilder = DriverMsg.newBuilder();
		driverMsgBuilder.setType(AmberProto.DriverMsg.MsgType.DATA);
		driverMsgBuilder.setExtension(NinedofProto.dataRequest,
				dataRequestBuilder.build());

		driverMsgBuilder.setSynNum(synNum);

		return driverMsgBuilder.build();
	}

	public NinedofData getAxesData(boolean accel, boolean gyro, boolean magnet)
			throws DriverTimeoutException, IOException {
		int synNum = getNextSynNum();

		DriverMsg dataRequestMsg = buildDataRequestMsg(synNum, accel, gyro,
				magnet);
		
		NinedofData ninedofData = new NinedofData();
		futureObjectsMap.put(synNum, ninedofData);
		
		amberClient.sendMessage(buildHeader(), dataRequestMsg);
		
		return ninedofData;
	}

	@Override
	public ExtensionRegistry getExtensionRegistry() {
		return extensionRegistry;
	}


}
