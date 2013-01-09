package ninedof.client;

import amber.client.FutureObject;

public class NinedofData extends FutureObject {
	
	public static class AxesData {
		public int xAxis;
		public int yAxis;
		public int zAxis;
		
		AxesData(int xAxis, int yAxis, int zAxis) {
			this.xAxis = xAxis;
			this.yAxis = yAxis;
			this.zAxis = zAxis;
		}
	}
	
	private AxesData accel;
	private AxesData gyro;
	private AxesData magnet;

	public AxesData getAccel() throws Exception {
		if (!available) {
			waitAvailable();
		}
		
		return accel;
	}
	
	public AxesData getGyro() throws Exception {
		if (!available) {
			waitAvailable();
		}
		
		return gyro;
	}
	
	public AxesData getMagnet() throws Exception {
		if (!available) {
			waitAvailable();
		}
		
		return magnet;
	}
	
	public void setAccel(AxesData axesData) {
		accel = axesData;
	}
	
	public void setGyro(AxesData axesData) {
		gyro = axesData;
	}
	
	public void setMagnet(AxesData axesData) {
		magnet = axesData;
	}
	
	
}
