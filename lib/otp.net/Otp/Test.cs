using System;

namespace Otp
{
	public class Test
	{
		static public void Main(String[] args)
		{
			OtpTrace.TraceEvent("Otp test...");

			if (args.Length < 1)
			{
				OtpTrace.TraceEvent("Usage: Otp sname\n  where sname is"+
					"the short name of the Erlang node");
				return;
			}

			String host = System.Net.Dns.GetHostName();
			String remote = args[0]+"@"+host;

			OtpNode node = new OtpNode("q@"+host);
			OtpTrace.TraceEvent("This node is called {0} and is using cookie='{1}'.",
				node.node(), node.cookie());
			bool ok=false;
			ok = node.ping(remote, 1000);
			if (ok)
				OtpTrace.TraceEvent("   successfully pinged node "+remote+"\n");
			else
				OtpTrace.TraceEvent("   could not ping node "+remote+"\n");

			OtpMbox mbox = null;

			try
			{
				mbox = node.createMbox();

				Erlang.Object[] rpc = new Erlang.Object[2];
				Erlang.Object[] call = new Erlang.Object[5];

				call[0] = new Erlang.Atom("call");
				call[1] = new Erlang.Atom("lists");
				call[2] = new Erlang.Atom("reverse");
				call[3] = new Erlang.List(new Erlang.List("Hello Erlang world!"));
				call[4] = mbox.self();

				rpc[0] = mbox.self();
				rpc[1] = new Erlang.Tuple(call);

				Erlang.Tuple rpcTuple = new Erlang.Tuple(rpc);
				OtpTrace.TraceEvent("=> "+rpcTuple.ToString());

				mbox.send("rex", remote, rpcTuple);
				Erlang.Object reply = mbox.receive(1000);

				OtpTrace.TraceEvent("<= "+reply.ToString());
			}
			catch (System.Exception)
			{
			}
			finally
			{
				node.closeMbox(mbox);
			}

			node.close();
		}
	}
}
