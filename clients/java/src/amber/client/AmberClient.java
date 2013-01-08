package amber.client;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.collections.keyvalue.MultiKey;

import amber.proto.AmberProto.DriverHdr;
import amber.proto.AmberProto.DriverMsg;

import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;

public class AmberClient implements Runnable {

	private final DatagramSocket socket;
	private final InetAddress address;
	private final int port;
	
	private final static int buffSize = 512;
	private static final int RECEIVING_TIMEOUT = 1000;
	
	private Map<MultiKey, AmberProxy> proxyMap = new HashMap<MultiKey, AmberProxy>();
	private Thread receivingThread;

	private static Logger logger = Logger.getLogger("AmberClient");
	
	public void registerClient(int deviceType, int deviceID, AmberProxy proxy) {
		proxyMap.put(new MultiKey(deviceType, deviceID), proxy);
	}
	
	public AmberClient(String hostname, int port) throws AmberConnectionException {
		
		try {
			this.socket = new DatagramSocket();
			this.address = InetAddress.getByName(hostname);
			this.port = port;
			
			receivingThread = new Thread(this);
			receivingThread.start();
			//logger.setLevel(Level.OFF);
			
		} catch (SocketException e) {
			throw new AmberConnectionException();
		} catch (UnknownHostException e) {
			throw new AmberConnectionException();
		}
	}
	
	@Override
	public void run() {
		messageReceivingLoop();		
	}

	private void messageReceivingLoop() {
		DatagramPacket packet = new DatagramPacket(new byte[buffSize], buffSize);	
		AmberProxy clientProxy = null;
		
		while (true) {
			try {
				socket.receive(packet);
				byte[] packetBytes = packet.getData();			
				
				int headerLen = (packetBytes[0] << 8) | packetBytes[1];			
				ByteString headerByteString = ByteString.copyFrom(packet.getData(), 2, headerLen);
				DriverHdr header = DriverHdr.parseFrom(headerByteString);
				
				int messageLen = (packetBytes[2 + headerLen] << 8) | packetBytes[2 + headerLen + 1];	
				ByteString messageByteString = ByteString.copyFrom(packet.getData(), 2 + headerLen + 2, messageLen);
				DriverMsg message;
				
				if (!header.hasDeviceType() || !header.hasDeviceID() || header.getDeviceType() == 0) {
					message = DriverMsg.parseFrom(messageByteString);
				} else {
					clientProxy = proxyMap.get(new MultiKey(header.getDeviceType(), header.getDeviceID()));
					
					if (clientProxy == null) {
						logger.info(
								String.format("Client proxy with given device type (%d) and ID (%d) not found. Ignoring message.", 
										header.getDeviceType(), header.getDeviceID()));
						continue;
					}
					
					message = DriverMsg.parseFrom(messageByteString, clientProxy.getExtensionRegistry());
					
				}
				
				clientProxy = proxyMap.get(new MultiKey(header.getDeviceType(), header.getDeviceID()));
				
				if (clientProxy == null) {
					logger.info(String.format("Client proxy with given device type (%d) and ID (%d) not found. Ignoring message.", 
							header.getDeviceType(), header.getDeviceID()));
					continue;
				}
				
				message = DriverMsg.parseFrom(messageByteString, clientProxy.getExtensionRegistry());
				
				switch (message.getType()) {
				case DATA:
					if (clientProxy == null) {
						logger.fine("DATA message came, but device details not set, ignoring.");
						continue;
					} else {
						clientProxy.handleDataMsg(header, message);	
					}					
					break;
					
				case PING:
					if (clientProxy == null) {
						handlePingMessage(header, message);
					} else {
						clientProxy.handlePingMsg(header, message);
					}
					break;
					
				case PONG:
					if (clientProxy == null) {
						handlePongMessage(header, message);
					} else {
						clientProxy.handlePongMsg(header, message);
					}
					break;
					
				case DRIVER_DIED:
					if (clientProxy == null) {
						logger.fine("DRIVER_DIED message came, but device details not set, ignoring.");
						continue;
					} else {
						clientProxy.handleDriverDiedMsg(header, message);
					}
					break;
					
				default:
					logger.fine("Unsupported message type came, ignoring");
					continue;
				}
				
			} catch (InvalidProtocolBufferException ex) {
				logger.fine("Cannot deserializer the message");
			} catch (IOException e) {
				logger.fine("IOException while receiving packet");
			}
		}
	}
	
	synchronized public void sendMessage(DriverHdr header, DriverMsg message)
			throws IOException {
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

		int len;

		// Header length
		len = header.getSerializedSize();
		outputStream.write((byte) (len >> 8) & 0xff);
		outputStream.write((byte) (len & 0xff));

		// Header
		outputStream.write(header.toByteArray());

		// Message length
		len = message.getSerializedSize();
		outputStream.write((byte) (len >> 8) & 0xff);
		outputStream.write((byte) (len & 0xff));

		// Message
		outputStream.write(message.toByteArray());

		// Send package
		//logger.info("Sending an UDP packet.");

		DatagramPacket packet = new DatagramPacket(outputStream.toByteArray(),
				outputStream.size(), address, port);
		socket.send(packet);
	}
	
	private void handlePingMessage(DriverHdr header, DriverMsg message) {
		
	}
	
	private void handlePongMessage(DriverHdr header, DriverMsg message) {
		
	}
	
}
