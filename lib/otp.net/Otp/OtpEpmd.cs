/*``The contents of this file are subject to the Erlang Public License,
* Version 1.1, (the "License"); you may not use this file except in
* compliance with the License. You should have received a copy of the
* Erlang Public License along with this software. If not, it can be
* retrieved via the world wide web at http://www.erlang.org/.
* 
* Software distributed under the License is distributed on an "AS IS"
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
* the License for the specific language governing rights and limitations
* under the License.
* 
* The Initial Developer of the Original Code is Ericsson Utvecklings AB.
* Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
* AB. All Rights Reserved.''
* 
 * Converted from Java to C# by Vlad Dumitrescu (vlad_Dumitrescu@hotmail.com)
*/
namespace Otp
{
	using System;
    using System.Net.Sockets;
	
	/*
	* Provides methods for registering, unregistering and looking up
	* nodes with the Erlang portmapper daemon (Epmd). For each registered
	* node, Epmd maintains information about the port on which incoming
	* connections are accepted, as well as which versions of the Erlang
	* communication protocolt the node supports.
	*
	* <p> Nodes wishing to contact other nodes must first request
	* information from Epmd before a connection can be set up, however
	* this is done automatically by {@link OtpSelf#connect(OtpPeer)
	* OtpSelf.connect()} when necessary.
	*
	* <p> The methods {@link #publishPort(OtpLocalNode) publishPort()} and
	* {@link #unPublishPort(OtpLocalNode) unPublishPort()} will fail if an
	* Epmd process is not running on the localhost. Additionally {@link
	* #lookupPort(AbstractNode) lookupPort()} will fail if there is no Epmd
	* process running on the host where the specified node is running.
	* See the Erlang documentation for information about starting Epmd.
	*
	* <p> This class contains only static methods, there are no
	* constructors.
	**/
	public class OtpEpmd
	{
		static OtpEpmd()
		{
			{
				// debug this connection?
				System.String trace = "0";
				try
				{
					if (trace != null)
						traceLevel = System.Int32.Parse(trace);
				}
				catch (System.FormatException)
				{
					traceLevel = 0;
				}
			}
		}
		// common values
		private const int epmdPort = 4369;
		private static readonly sbyte stopReq = (sbyte) 115;
		
		// version specific value
		private static readonly sbyte port3req = (sbyte) 112;
		private static readonly sbyte publish3req = (sbyte) 97;
		private static readonly sbyte publish3ok = (sbyte) 89;
		
		private static readonly sbyte port4req = (sbyte) 122;
		private static readonly sbyte port4resp = (sbyte) 119;
		private static readonly sbyte publish4req = (sbyte) 120;
		private static readonly sbyte publish4resp = (sbyte) 121;
		
		private static int traceLevel = 0;
		private const int traceThreshold = 4;

		// only static methods: no public constructors
		// hmm, idea: singleton constructor could spawn epmd process
		private OtpEpmd()
		{
		}
		
		/*
		* Determine what port a node listens for incoming connections on.
		*
		* @return the listen port for the specified node, or 0 if the node
		* was not registered with Epmd.
		*
		* @exception C#.io.IOException if there was no response from the
		* name server.
		**/
		public static int lookupPort(AbstractNode node)
		{
			try
			{
				return r4_lookupPort(node);
			}
			catch (System.IO.IOException)
			{
				return r3_lookupPort(node);
			}
		}
		
		/*
		* Register with Epmd, so that other nodes are able to find and
		* connect to it. 
		*
		* @param node the server node that should be registered with Epmd.
		*
		* @return true if the operation was successful. False if the node
		* was already registered.
		*
		* @exception C#.io.IOException if there was no response from the
		* name server.
		**/
		public static bool publishPort(OtpLocalNode node)
		{
			System.Net.Sockets.TcpClient s = null;
			
			try
			{
				s = r4_publish(node);
			}
			catch (System.IO.IOException)
			{
				s = r3_publish(node);
			}
			
			node.setEpmd(s);
			
			return (s != null);
		}
		
		// Ask epmd to close his end of the connection.
		// Caller should close his epmd socket as well.
		// This method is pretty forgiving...
		/*
		* Unregister from Epmd. Other nodes wishing to connect will no
		* longer be able to.
		*
		* <p> This method does not report any failures.
		**/
		public static void  unPublishPort(OtpLocalNode node)
		{
			TcpClient s = null;
			
			try
			{
				s = new TcpClient(System.Net.Dns.GetHostName(), epmdPort);
				OtpOutputStream obuf = new OtpOutputStream();
				obuf.write2BE(node.getAlive().Length + 1);
				obuf.write1(stopReq);
				//UPGRADE_NOTE: This code will be optimized in the future;
				byte[] tmpBytes;
				int i;
				string tmpStr;
				tmpStr = node.getAlive();
				tmpBytes = new byte[tmpStr.Length];
				i = 0;
				while (i < tmpStr.Length)
				{
					tmpBytes[i] = (byte) tmpStr[i];
					i++;
				}
				obuf.writeN(tmpBytes);
				obuf.writeTo((System.IO.Stream) s.GetStream());
				// don't even wait for a response (is there one?) 
				if (traceLevel >= traceThreshold)
				{
					OtpTrace.TraceEvent("-> UNPUBLISH " + node + " port=" + node.port());
					OtpTrace.TraceEvent("<- OK (assumed)");
				}
			}
			catch (System.Exception)
			{
				/*ignore all failures */
			}
			finally
			{
				try
				{
					if (s != null)
						s.Close();
				}
				catch (System.IO.IOException)
				{
					/*ignore close failure */
				}
				s = null;
			}
		}
		
		private static int r3_lookupPort(AbstractNode node)
		{
			int port = 0;
			System.Net.Sockets.TcpClient s = null;
			
			try
			{
				OtpOutputStream obuf = new OtpOutputStream();
				s = new System.Net.Sockets.TcpClient(node.host(), epmdPort);
				
				// build and send epmd request
				// length[2], tag[1], alivename[n] (length = n+1)
				obuf.write2BE(node.getAlive().Length + 1);
				obuf.write1(port3req);
				//UPGRADE_NOTE: This code will be optimized in the future;
				byte[] tmpBytes;
				int i;
				string tmpStr;
				tmpStr = node.getAlive();
				tmpBytes = new byte[tmpStr.Length];
				i = 0;
				while (i < tmpStr.Length)
				{
					tmpBytes[i] = (byte) tmpStr[i];
					i++;
				}
				obuf.writeN(tmpBytes);
				
				// send request
				obuf.writeTo((System.IO.Stream) s.GetStream());
				
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("-> LOOKUP (r3) " + node);
				
				// receive and decode reply
				byte[] tmpbuf = new byte[100];

				s.GetStream().Read(tmpbuf, 0, 100);
				OtpInputStream ibuf = new OtpInputStream(tmpbuf);
				
				port = ibuf.read2BE();
			}
			catch (System.IO.IOException)
			{
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("<- (no response)");
				throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when looking up " + node.getAlive());
			}
			catch (Erlang.DecodeException)
			{
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("<- (invalid response)");
				throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when looking up " + node.getAlive());
			}
			finally
			{
				try
				{
					if (s != null)
						s.Close();
				}
				catch (System.IO.IOException)
				{
					/*ignore close errors */
				}
				s = null;
			}
			
			if (traceLevel >= traceThreshold)
			{
				if (port == 0)
					OtpTrace.TraceEvent("<- NOT FOUND");
				else
					OtpTrace.TraceEvent("<- PORT " + port);
			}
			return port;
		}
		
		private static int r4_lookupPort(AbstractNode node)
		{
			int port = 0;
			System.Net.Sockets.TcpClient s = null;
			
			try
			{
				OtpOutputStream obuf = new OtpOutputStream();
				s = new System.Net.Sockets.TcpClient(node.host(), epmdPort);
				
				// build and send epmd request
				// length[2], tag[1], alivename[n] (length = n+1)
				obuf.write2BE(node.getAlive().Length + 1);
				obuf.write1(port4req);
				//UPGRADE_NOTE: This code will be optimized in the future;
				byte[] tmpBytes;
				int i;
				string tmpStr;
				tmpStr = node.getAlive();
				tmpBytes = new byte[tmpStr.Length];
				i = 0;
				while (i < tmpStr.Length)
				{
					tmpBytes[i] = (byte) tmpStr[i];
					i++;
				}
				obuf.writeN(tmpBytes);
				
				// send request
				obuf.writeTo((System.IO.Stream) s.GetStream());
				
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("-> LOOKUP (r4) " + node);
				
				// receive and decode reply
				// resptag[1], result[1], port[2], ntype[1], proto[1],
				// disthigh[2], distlow[2], nlen[2], alivename[n],
				// elen[2], edata[m]
				byte[] tmpbuf = new byte[100];

				int n = s.GetStream().Read(tmpbuf, 0, 100);
				
				if (n < 0)
				{
					// this was an r3 node => not a failure (yet)
					if (s != null)
						s.Close();
					throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when looking up " + node.getAlive());
				}
				
				OtpInputStream ibuf = new OtpInputStream(tmpbuf);
				
				int response = ibuf.read1();
				if (response == port4resp)
				{
					int result = ibuf.read1();
					if (result == 0)
					{
						port = ibuf.read2BE();
						
						node.ntype = ibuf.read1();
						node._proto = ibuf.read1();
						node._distHigh = ibuf.read2BE();
						node._distLow = ibuf.read2BE();
						// ignore rest of fields
					}
				}
			}
			catch (System.IO.IOException)
			{
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("<- (no response)");
				throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when looking up " + node.getAlive());
			}
			catch (Erlang.DecodeException)
			{
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("<- (invalid response)");
				throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when looking up " + node.getAlive());
			}
			finally
			{
				try
				{
					if (s != null)
						s.Close();
				}
				catch (System.IO.IOException)
				{
					/*ignore close errors */
				}
				s = null;
			}
			
			if (traceLevel >= traceThreshold)
			{
				if (port == 0)
					OtpTrace.TraceEvent("<- NOT FOUND");
				else
					OtpTrace.TraceEvent("<- PORT " + port);
			}
			return port;
		}
		
		private static System.Net.Sockets.TcpClient r3_publish(OtpLocalNode node)
		{
			System.Net.Sockets.TcpClient s = null;
			
			try
			{
				OtpOutputStream obuf = new OtpOutputStream();
				s = new System.Net.Sockets.TcpClient(System.Net.Dns.GetHostName(), epmdPort);
				
				obuf.write2BE(node.getAlive().Length + 3);
				
				obuf.write1(publish3req);
				obuf.write2BE(node.port());
				//UPGRADE_NOTE: This code will be optimized in the future;
				byte[] tmpBytes;
				int i;
				string tmpStr;
				tmpStr = node.getAlive();
				tmpBytes = new byte[tmpStr.Length];
				i = 0;
				while (i < tmpStr.Length)
				{
					tmpBytes[i] = (byte) tmpStr[i];
					i++;
				}
				obuf.writeN(tmpBytes);
				
				// send request
				obuf.writeTo((System.IO.Stream) s.GetStream());
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("-> PUBLISH (r3) " + node + " port=" + node.port());
				
				byte[] tmpbuf = new byte[100];

				int n = s.GetStream().Read(tmpbuf, 0, 100);
				
				if (n < 0)
				{
					if (s != null)
						s.Close();
					if (traceLevel >= traceThreshold)
						OtpTrace.TraceEvent("<- (no response)");
					return null;
				}
				
				OtpInputStream ibuf = new OtpInputStream(tmpbuf);
				
				if (ibuf.read1() == publish3ok)
				{
					node._creation = ibuf.read2BE();
					if (traceLevel >= traceThreshold)
						OtpTrace.TraceEvent("<- OK");
					return s; // success - don't close socket
				}
			}
			catch (System.IO.IOException)
			{
				// epmd closed the connection = fail
				if (s != null)
					s.Close();
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("<- (no response)");
				throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when publishing " + node.getAlive());
			}
			catch (Erlang.DecodeException)
			{
				if (s != null)
					s.Close();
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("<- (invalid response)");
				throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when publishing " + node.getAlive());
			}
			
			if (s != null)
				s.Close();
			return null; // failure
		}
		
		/*this function will get an exception if it tries to talk to an r3
		* epmd, or if something else happens that it cannot forsee. In both
		* cases we return an exception (and the caller should try again, using
		* the r3 protocol).
		* If we manage to successfully communicate with an r4 epmd, we return
		* either the socket, or null, depending on the result.
		*/
		private static System.Net.Sockets.TcpClient r4_publish(OtpLocalNode node)
		{
			System.Net.Sockets.TcpClient s = null;
            System.Exception error = null;
			
			try
			{
				OtpOutputStream obuf = new OtpOutputStream();
				s = new TcpClient(System.Net.Dns.GetHostName(), epmdPort);
				
				obuf.write2BE(node.getAlive().Length + 13);
				
				obuf.write1(publish4req);
				obuf.write2BE(node.port());
				
				obuf.write1(node.type());
				
				obuf.write1(node.proto());
				obuf.write2BE(node.distHigh());
				obuf.write2BE(node.distLow());
				
				obuf.write2BE(node.getAlive().Length);
				//UPGRADE_NOTE: This code will be optimized in the future;
				byte[] tmpBytes;
				int i;
				string tmpStr;
				tmpStr = node.getAlive();
				tmpBytes = new byte[tmpStr.Length];
				i = 0;
				while (i < tmpStr.Length)
				{
					tmpBytes[i] = (byte) tmpStr[i];
					i++;
				}
				obuf.writeN(tmpBytes);
				obuf.write2BE(0); // No extra
				
				// send request
				obuf.writeTo((System.IO.Stream) s.GetStream());
				
				if (traceLevel >= traceThreshold)
					OtpTrace.TraceEvent("-> PUBLISH (r4) " + node + " port=" + node.port());
				
				// get reply
				byte[] tmpbuf = new byte[100];
				int n = s.GetStream().Read(tmpbuf, 0, 100);
				
				if (n < 0)
				{
					// this was an r3 node => not a failure (yet)
					if (s != null)
						s.Close();
					throw new System.IO.IOException("Nameserver not responding on " + node.host() + " when publishing " + node.getAlive());
				}
				
				OtpInputStream ibuf = new OtpInputStream(tmpbuf);
				
				int response = ibuf.read1();
				if (response == publish4resp)
				{
					int result = ibuf.read1();
					if (result == 0)
					{
						node._creation = ibuf.read2BE();
						if (traceLevel >= traceThreshold)
							OtpTrace.TraceEvent("<- OK");
						return s; // success
					}
				}
			}
            catch (System.IO.IOException e)
            {
                error = e;
            }
            catch (Erlang.DecodeException e)
            {
                error = e;
            }
            catch (System.Net.Sockets.SocketException e)
            {
                error = e;
            }

            if (error == null)
                return s;
            else
            {
                // epmd closed the connection = fail
                if (s != null)
                    s.Close();
                if (traceLevel >= traceThreshold)
                    System.Console.Out.WriteLine("<- (no response)");

                string err = "Nameserver not responding on " + node.host() + " when publishing " + node.getAlive();
                node.epmdFailedConnAttempt(node.node(), err);

                if (traceLevel >= traceThreshold)
                    System.Console.Out.WriteLine("Failed to connect to empd daemon!");

                if (!OtpLocalNode.ignoreLocalEpmdConnectErrors)
                    throw new System.Exception(err);

                node._creation = 0;
                return null;
            }
		}
	}
}