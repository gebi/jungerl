using System;
using System.Collections.Generic;
using System.Text;
using Otp;

namespace OtpTest1
{
    class Test1
    {
		static public void Main(String[] args)
		{
			System.Console.Out.WriteLine("Otp test...");

            string cookie = OtpNode.defaultCookie;

            AbstractConnection.traceLevel = OtpTrace.Type.sendThreshold;

			if (args.Length < 1)
			{
				System.Console.Out.WriteLine(
                    "Usage: {0} nodename [cookie] [-notrace]\n"+
                    "    nodename  - is the name of the remote Erlang node\n"+
                    "    cookie    - is the optional cookie string to use\n"+
                    "    -notrace  - disable debug trace\n", 
                    Environment.GetCommandLineArgs()[0]);
				return;
            }
            else if (args.Length > 1)
            {
                cookie = args[1].ToString();
            }

            for (int i=0; i < args.Length; i++)
                if (args[i].Equals("-notrace"))
                {
                    AbstractConnection.traceLevel = OtpTrace.Type.defaultLevel;
                    break;
                }

    
			String host = System.Net.Dns.GetHostName();
			String remote = (args[0].IndexOf('@') < 0) ? args[0]+"@"+host : args[0];

            OtpNode node = new OtpNode(false, Environment.UserName + "123@" + host, cookie, true);

			System.Console.Out.WriteLine("This node is called {0} and is using cookie='{1}'.",
				node.node(), node.cookie());
			
            bool ok=node.ping(remote, 1000);

            if (!ok)
            {
                Console.WriteLine("Can't connect to node " + remote);
                return;
            }

            // If using short names, get the short name of the peer.
            remote = node.connection(remote).peer.node();

            if (remote != null)
				System.Console.Out.WriteLine("   successfully pinged node "+remote+"\n");
			else
				System.Console.Out.WriteLine("   could not ping node "+remote+"\n");

			OtpMbox mbox = null;

			try
            {
                mbox = node.createMbox();

                {
                    Otp.Erlang.Object reply = mbox.rpcCall(
                        remote, "lists", "reverse", new Otp.Erlang.List("Abcdef!"));
                    System.Console.Out.WriteLine("<= [REPLY1]:" + (reply == null ? "null" : reply.ToString()));
                }

                {
                    Otp.Erlang.Object reply = mbox.rpcCall(
                        remote, "global", "register_name", 
                        new Otp.Erlang.List(new Otp.Erlang.Atom("me"), mbox.self()));

                    System.Console.Out.WriteLine("<= [REPLY2]:" + (reply == null ? "null" : reply.ToString()));
                }

                {
                    Otp.Erlang.Object reply = mbox.rpcCall(remote, "global", "register_name", new Otp.Erlang.List(new Otp.Erlang.Atom("me"), mbox.self()), 5000);
                    System.Console.Out.WriteLine("<= [REPLY3]:" + (reply == null ? "null" : reply.ToString()));
                }

                {
                    Otp.Erlang.Object reply = mbox.rpcCall(
                        remote, "io", "format",
                        new Otp.Erlang.List(
                            "Test: ~w -> ~w\n",
                            new Otp.Erlang.List(mbox.self(), new Otp.Erlang.Atom("ok"))
                        ));

                    System.Console.Out.WriteLine("<= [REPLY4]:" + (reply == null ? "null" : reply.ToString()));
                }

                while (true)
                {
                    Otp.Erlang.Object msg = mbox.receive();
                    if (msg is Otp.Erlang.Tuple)
                    {
                        Otp.Erlang.Tuple m = msg as Otp.Erlang.Tuple;
                        if (m.arity() == 2 && m.elementAt(0) is Otp.Erlang.Pid)
                        {
                            mbox.send(m.elementAt(0) as Otp.Erlang.Pid, m.elementAt(1));
                        }
                    }
                    System.Console.Out.WriteLine("IN msg: " + msg.ToString() + "\n");
                }

            }
			catch (System.Exception e)
			{
                System.Console.Out.WriteLine("Error: " + e.ToString());
			}
			finally
			{
				node.closeMbox(mbox);
			}

			node.close();
		}
	}
}
