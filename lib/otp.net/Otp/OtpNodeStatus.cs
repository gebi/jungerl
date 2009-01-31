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
    /*
    * <p> Provides a callback mechanism for receiving status change
    * information about other nodes in the system. Register an instance
    * of this class (or a subclass) with your {@link OtpNode OtpNode}
    * when you wish to be notified about such status changes and other
    * similar events.</p>
    *
    * <p> This class provides default handers that ignore all events.
    * Applications are expected to resiter a delagate that would
    * react on node status change events that are deemed interesting.</p>
    *
    * <p> <b> Note that this class is likely to change in the near future
    * </b></p>
    **/

    public class OtpNodeStatus
    {
        public OtpNodeStatus()
        {
        }

        public enum EventCategory { Local, Remote, ConnectionAttempt, Epmd };
        public enum EventType { Down, Up, Incoming, Outgoing };

        public delegate void ConnectionStatusDelegate(
            System.String node, EventCategory type, EventType ev, System.Object info);

        private ConnectionStatusDelegate onConnStatus;

        public void registerStatusHandler(ConnectionStatusDelegate callback)
        {
            onConnStatus += callback;
        }

        public void unregisterStatusHandler(ConnectionStatusDelegate callback)
        {
            if (onConnStatus != null)
                onConnStatus -= callback;
        }

        /*
        * Notify about remote node status changes.
        *
        * @param node the node whose status change is being indicated by
        * this call.
        *
        * @param up true if the node has come up, false if it has gone
        * down.
        *
        * @param info additional info that may be available, for example an
        * exception that was raised causing the event in question (may be
        * null).
        *
        **/

        public virtual void remoteStatus(System.String node, bool up, System.Object info)
        {
            if (onConnStatus != null)
                onConnStatus(node, EventCategory.Remote, up ? EventType.Up : EventType.Down, info);
        }

        /*
        * Notify about local node exceptions.
        *
        * @param node the node whose status change is being indicated by
        * this call.
        *
        * @param up true if the node has come up, false if it has gone
        * down.
        *
        * @param info additional info that may be available, for example an
        * exception that was raised causing the event in question (may be
        * null).
        **/
        public virtual void localStatus(System.String node, bool up, System.Object info)
        {
            if (onConnStatus != null)
                onConnStatus(node, EventCategory.Local, up ? EventType.Up : EventType.Down, info);
        }

        /*
        * Notify about failed connection attempts.
        *
        * @param node The name of the remote node
        *
        * @param incoming The direction of the connection attempt, i.e.
        * true for incoming, false for outgoing.
        *
        * @param info additional info that may be available, for example an
        * exception that was raised causing the event in question (may be
        * null).
        **/
        public virtual void connAttempt(System.String node, bool incoming, System.Object info)
        {
            if (onConnStatus != null)
                onConnStatus(node, EventCategory.ConnectionAttempt,
                    incoming ? EventType.Incoming : EventType.Outgoing, info);
        }

        /*
        * Notify about failed connection attempt to epmd.
        *
        * @param node The name of the local node
        *
        * @param info additional info that may be available, for example an
        * exception that was raised causing the event in question (may be
        * null).
        **/
        public virtual void epmdFailedConnAttempt(System.String node, System.Object info)
        {
            if (onConnStatus != null)
                onConnStatus(node, EventCategory.Epmd, EventType.Down, info);
        }
    }
}