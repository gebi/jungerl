using System;

namespace Otp
{
    public class Test
    {
        static public void Main(String[] args)
        {
            System.Console.Out.WriteLine("Otp test...");

            if (args.Length < 1)
            {
                System.Console.Out.WriteLine("Usage: Otp sname\n  where sname is" +
                    "the short name of the Erlang node");
                return;
            }

            OtpNode.useShortNames = true;

            String  host   = System.Net.Dns.GetHostName();
            String  user   = Environment.UserName;
            OtpNode node   = new OtpNode(user + "@" + host);
            String  remote = (args[0].Contains("@")) ? args[0] : remote = args[0] + "@" + host;
            OtpMbox mbox   = null;

            System.Console.Out.WriteLine("This node is: {0} (cookie='{1}'). Remote: {2}",
                node.node(), node.cookie(), remote);
            
            //bool ok = node.ping(remote, 1000*300);

            OtpCookedConnection conn = node.getConnection(remote);

            try
            {
                if (conn != null)
                    System.Console.Out.WriteLine("   successfully pinged node " + remote + "\n");
                else
                    throw new System.Exception("Could not ping node: " + remote);

                conn.traceLevel = 1;

                mbox = node.createMbox();
                mbox.registerName("server");

                mbox.sendRPC(conn.peer.node(), "lists", "reverse", new Erlang.List(new Erlang.String("Hello world!")));
				Erlang.Object reply = mbox.receiveRPC(5000);
                System.Console.Out.WriteLine("<= " + reply.ToString());

                {
                    Erlang.List rpcArgs = new Erlang.List(
                        new Erlang.Object[] {
                            mbox.self(),
                            new Erlang.Tuple(
                                new Erlang.Object[] {
                                    new Erlang.Atom("table"), new Erlang.Atom("test"), new Erlang.Atom("simple")
                                }
                            )
                        }
                    );

                    mbox.sendRPC(conn.peer.node(), "mnesia_subscr", "subscribe", rpcArgs);
				    reply = mbox.receiveRPC(5000);
                    System.Console.Out.WriteLine("<= " + reply.ToString());
                }

                while (true)
                {
                    Erlang.Object msg = mbox.receive();
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
