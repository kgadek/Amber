package ninedof.client;

import amber.client.AmberClient;
import amber.client.AmberConnectionException;
import amber.client.AmberListener;

public class NinedofClientDemo implements AmberListener<NinedofData> {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		(new NinedofClientDemo()).runDemo();		
	}
	
	public void runDemo() {
		long before, after;
		
		AmberClient client;
		try {
			client = new AmberClient("localhost", 26233);
			Thread.sleep(1000);
			NinedofProxy ninedofProxy = new NinedofProxy(client, 0);
			
			for (int i = 0; i < 5; i++) {
				before = System.nanoTime();
				
				NinedofData data = ninedofProxy.getAxesData(true, true, true);
			
				data.waitAvailable();
				
				after = System.nanoTime();
				
				System.out.println(String.format("accel x: %s, y: %s, z: %s", 
						data.getAccel().xAxis, data.getAccel().yAxis, data.getAccel().zAxis));
				
				System.out.println(String.format("gyro x: %s, y: %s, z: %s", 
						data.getGyro().xAxis, data.getGyro().yAxis, data.getGyro().zAxis));
				
				System.out.println(String.format("magnet x: %s, y: %s, z: %s", 
						data.getMagnet().xAxis, data.getMagnet().yAxis, data.getMagnet().zAxis));	
			
				System.out.println("time: " + (after - before)/1000 + "us");
			}			
			
			ninedofProxy.registerNinedofDataListener(1000, true, true, true, this);
			
			Thread.sleep(10000);
			
			ninedofProxy.unregisterDataListener();
			
		} catch (AmberConnectionException e) {
			System.err.println("Connection error: " + e);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void handle(NinedofData data) {
		System.out.println("Got new cyclic message.");
		
		try {
			System.out.println(String.format("accel x: %s, y: %s, z: %s", 
					data.getAccel().xAxis, data.getAccel().yAxis, data.getAccel().zAxis));
			
			System.out.println(String.format("gyro x: %s, y: %s, z: %s", 
					data.getGyro().xAxis, data.getGyro().yAxis, data.getGyro().zAxis));
			
			System.out.println(String.format("magnet x: %s, y: %s, z: %s", 
					data.getMagnet().xAxis, data.getMagnet().yAxis, data.getMagnet().zAxis));	
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
}
