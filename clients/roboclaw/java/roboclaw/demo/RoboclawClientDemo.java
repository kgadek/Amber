package roboclaw.demo;

import roboclaw.client.MotorsCommand;
import roboclaw.client.RoboclawProxy;
import amber.client.AmberClient;
import amber.client.AmberConnectionException;

public class RoboclawClientDemo {

	public static void main(String[] args) {
		(new RoboclawClientDemo()).runDemo();
	}
	
	public void runDemo() {
		
		AmberClient client;
		try {
			client = new AmberClient("localhost", 26233);
			
			RoboclawProxy roboclawProxy = new RoboclawProxy(client, 0);
		
			MotorsCommand mc1 = new MotorsCommand();
			mc1.m1Speed = 1000;
			mc1.m2Speed = 1000;
			mc1.address = (byte)0x80;
			
			MotorsCommand mc2 = new MotorsCommand();
			mc2.m1Speed = 2000;
			mc2.m2Speed = 2000;
			mc2.address = (byte)0x81;
			
			roboclawProxy.sendMotorsEncoderCommand(mc1, mc2);			
			
		} catch (AmberConnectionException e) {
			System.err.println("Connection error: " + e);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
