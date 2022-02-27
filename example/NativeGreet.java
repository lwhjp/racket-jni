package example;

class NativeGreet {

	public String greet() {
		return "hello, " + getName();
	}

	private native String getName();

}
