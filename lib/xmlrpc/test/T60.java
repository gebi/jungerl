import org.apache.xmlrpc.*;
import java.util.*;

public class T60 {
    public static void main(String[] args) {
	try {
	    XmlRpcClient xmlrpc = new XmlRpcClient("http://localhost:4567/");
	    XmlRpc.setKeepAlive(true);
	    Vector params = new Vector();
	    Vector v = new Vector();
	    v.addElement(new Integer(42));
	    v.addElement(new String("foo"));
	    params.add(v);
	    Hashtable h = new Hashtable();
	    h.put("bar", new Double(45.5));
	    h.put("baz", new Integer(4711));
	    params.add(h);
	    Object response = xmlrpc.execute("echo", params);
	    System.out.println(response.toString());
	    response = xmlrpc.execute("echo", params);
	    System.out.println(response.toString());
	    response = xmlrpc.execute("foo", new Vector());
	    System.out.println(response.toString());
	} catch (Exception e) {
	    System.err.println(e);
	}
    }
}
