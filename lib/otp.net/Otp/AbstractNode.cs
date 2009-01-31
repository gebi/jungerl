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
using System;
using System.Configuration;

namespace Otp
{
    
    /*
    * <p> Represents an OTP node. </p>
    * 
    * <p> About nodenames: Erlang nodenames consist of two components, an
    * alivename and a hostname separated by '@'. Additionally, there are
    * two nodename formats: short and long. Short names are of the form
    * "alive@hostname", while long names are of the form
    * "alive@host.fully.qualified.domainname". Erlang has special
    * requirements regarding the use of the short and long formats, in
    * particular they cannot be mixed freely in a network of
    * communicating nodes, however Jinterface makes no distinction. See
    * the Erlang documentation for more information about nodenames. </p>
    * 
    * <p> The constructors for the AbstractNode classes will create names
    * exactly as you provide them as long as the name contains '@'. If
    * the string you provide contains no '@', it will be treated as an
    * alivename and the name of the local host will be appended,
    * resulting in a shortname. Nodenames longer than 255 characters will
    * be truncated without warning. </p>
    *
    * <p> Upon initialization, this class attempts to read the file
    * .erlang.cookie in the user's home directory, and uses the trimmed
    * first line of the file as the default cookie by those constructors
    * lacking a cookie argument. If for any reason the file cannot be
    * found or read, the default cookie will be set to the empty string
    * (""). The location of a user's home directory is determined using
    * the system property "user.home", which may not be automatically set
    * on all platforms. </p>
    *
    * <p> Instances of this class cannot be created directly, use one of
    * the subclasses instead. </p>
    **/
    public class AbstractNode
    {
        private void  InitBlock()
        {
            //ntype = NTYPE_R6;
            //flags = dFlagExtendedReferences | dFlagExtendedPidsPorts;
        }
        static AbstractNode()
        {
            {
                try
                {
                    localHost = System.Net.Dns.GetHostName();
                }
                catch (System.Exception)
                {
                    localHost = "localhost";
                }

                try
                {
                    defaultCookie = ConfigurationManager.AppSettings["ErlangCookie"];
                }
                catch
                {
                    defaultCookie = null;
                }

                if (defaultCookie == null)
                {
                    System.String dotCookieFilename = System.Environment.GetEnvironmentVariable("HOME")
                        + System.IO.Path.DirectorySeparatorChar
                        + ".erlang.cookie";
                    System.IO.StreamReader br = null;
                    try
                    {
                        System.IO.FileInfo dotCookieFile = new System.IO.FileInfo(dotCookieFilename);
                        br = new System.IO.StreamReader(new System.IO.StreamReader(dotCookieFile.FullName).BaseStream);
                        defaultCookie = br.ReadLine().Trim();
                    }
                    catch (System.IO.IOException)
                    {
                        defaultCookie = null;
                    }
                    finally
                    {
                        try
                        {
                            if (br != null)
                                br.Close();
                        }
                        catch (System.IO.IOException)
                        {
                        }
                    }
                }

                if (defaultCookie == null)
                    defaultCookie = string.Empty;
            }
        }
        internal static System.String localHost = null;
        internal System.String _node;
        internal System.String _host;
        internal System.String _alive;
        internal System.String _cookie;
        internal System.String _longName;
        public static string   defaultCookie = null;
        public static bool     useShortNames = false;

        // Node types
        internal const int NTYPE_R6 = 110; // 'n' post-r5, all nodes
        internal const int NTYPE_R4_ERLANG = 109;
        // 'm' Only for source compatibility
        internal const int NTYPE_R4_HIDDEN = 104;
        // 'h' Only for source compatibility

        // Node capability flags
        internal const int dFlagPublished = 1;
        internal const int dFlagAtomCache = 2;
        internal const int dFlagExtendedReferences = 4;
        internal const int dFlagDistMonitor = 8;
        internal const int dFlagFunTags = 16;
        internal const int dFlagExtendedPidsPorts = 256; // pshaffer
        internal const int dFlagBitBinaries = 1024;
        internal const int dFlagNewFloats = 2048;

        internal int ntype = NTYPE_R6;   // pshaffer
        internal int _proto = 0; // tcp/ip
        internal int _distHigh = 5; // Cannot talk to nodes before R6
        internal int _distLow = 5; // Cannot talk to nodes before R6
        internal int _creation = 0;
        internal int flags = dFlagExtendedReferences | dFlagExtendedPidsPorts
                           | dFlagBitBinaries | dFlagNewFloats | dFlagDistMonitor;  // pshaffer
        
        /*initialize hostname and default cookie */
        protected internal AbstractNode()
        {
            InitBlock();
        }
        
        /*
        * Create a node with the given name and the default cookie.
        **/
        protected internal AbstractNode(System.String node)
            : this(node, defaultCookie, false)
        {
        }

        /*
        * Create a node with the given name and cookie.
        **/
        protected internal AbstractNode(System.String name, System.String cookie, bool shortName)
        {
            InitBlock();
            this._cookie = cookie;

            int i = name.IndexOf((System.Char)'@', 0);
            if (i < 0)
            {
                _alive = name;
                _host = localHost;
            }
            else
            {
                _alive = name.Substring(0, (i) - (0));
                _host = name.Substring(i + 1, (name.Length) - (i + 1));
            }

            if (_alive.Length > 0xff)
            {
                _alive = _alive.Substring(0, (0xff) - (0));
            }

            _longName = _alive + "@" + _host;
            _node = node(_longName, shortName);
        }

        public static string node(System.String _node, bool _shortName)
        {
            if (_shortName || useShortNames)
            {
                int i = _node.IndexOf('@');
                i = i < 0 ? 0 : i + 1;
                int j = _node.IndexOf((System.Char)'.', i);
                return (j < 0) ? _node : _node.Substring(0, i + j - 2);
            }
            else
            {
                return _node;
            }
        }

        /*
        * Get the name of this node.
        *
        * @return the name of the node represented by this object.
        **/
        public string node()
        {
            return useShortNames ? _node : _longName;
        }

        public string nodeLongName()
        {
            return _longName;
        }

        /*
        * Get the hostname part of the nodename. Nodenames are composed of
        * two parts, an alivename and a hostname, separated by '@'. This
        * method returns the part of the nodename following the '@'.
        *
        * @return the hostname component of the nodename.
        **/
        public virtual System.String host()
        {
            return _host;
        }
        
        /*
        * Get the alivename part of the hostname. Nodenames are composed of
        * two parts, an alivename and a hostname, separated by '@'. This
        * method returns the part of the nodename preceding the '@'.
        *
        * @return the alivename component of the nodename.
        **/
        public virtual System.String getAlive()
        {
            return _alive;
        }
        
        /*
        * Get the authorization cookie used by this node.
        *
        * @return the authorization cookie used by this node.
        **/
        public virtual System.String cookie()
        {
            return _cookie;
        }
        
        // package scope
        internal virtual int type()
        {
            return ntype;
        }
        
        // package scope
        internal virtual int distHigh()
        {
            return _distHigh;
        }
        
        // package scope
        internal virtual int distLow()
        {
            return _distLow;
        }
        
        // package scope: useless information?
        internal virtual int proto()
        {
            return _proto;
        }
        
        // package scope
        internal virtual int creation()
        {
            return _creation;
        }
        
        /*
        * Set the authorization cookie used by this node.
        *
        * @return the previous authorization cookie used by this node.
        **/
        public virtual System.String setCookie(System.String cookie)
        {
            System.String prev = this._cookie;
            this._cookie = cookie;
            return prev;
        }
        
        public override System.String ToString()
        {
            return node();
        }
    }
}