package amber.client;

public interface AmberListener<T> {
	
	void handle(T data);

}
