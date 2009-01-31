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
	/*This class implements a generic FIFO queue. There is no upper
	* bound on the length of the queue, items are linked.
	*/
	using System;
	
	public class GenericQueue
	{
		private const int open = 0;
		private const int closing = 1;
		private const int closed = 2;
		
		private int status = closed;
		private Bucket head;
		private Bucket tail;
		private int count;
		
		private void  init()
		{
			head = null;
			tail = null;
			count = 0;
		}
		
		/*Create an empty queue */
		public GenericQueue()
		{
            init();
            status = open;
        }
		
		/*Clear a queue */
		public virtual void  flush()
		{
            close();
			init();
            status = open;
		}
		
		public virtual void  close()
		{
			status = closing;
            init();
            try
            {
                System.Threading.Monitor.PulseAll(this);
            }
            catch
            { 
            }
            status = closed;
        }
		
		/*Add an object to the tail of the queue.
		* @param o Object to insert in the queue
		*/
		public virtual void  put(System.Object o)
		{
			lock(this)
			{
				Bucket b = new Bucket(this, o);
				
				if (tail != null)
				{
					tail.setNext(b);
					tail = b;
				}
				else
				{
					// queue was empty but has one element now
					head = (tail = b);
				}
				count++;
				
				// notify any waiting tasks
                //UPGRADE_TODO: threading!
				System.Threading.Monitor.PulseAll(this);
			}
		}
		
        private System.Object get(bool once)
        {
			lock(this)
			{
				System.Object o = null;
				
				while ((o = tryGet()) == null && !once && status == open)
				{
					try
					{
						//UPGRADE_TODO: threading!
						System.Threading.Monitor.Wait(this);
					}
					catch (System.Threading.ThreadInterruptedException)
					{
					}
				}
				return o;
			}
        }
		/*Retrieve an object from the head of the queue, or block until
		* one arrives.
		*
		* @return  The object at the head of the queue.
		*/
		public virtual System.Object get()
		{
            return get(false);
		}
		
		/*Retrieve an object from the head of the queue, blocking until
		* one arrives or until timeout occurs.
		*
		* @param  timeout Maximum time to block on queue, in ms. Use 0 to poll the queue. 
		*
		* @exception InterruptedException if the operation times out.
		*
		* @return  The object at the head of the queue, or null if none arrived in time.
		*/
		public virtual System.Object get(long timeout)
		{
            if (timeout == -1)
                return get(false);
            else if (timeout == 0)
                return get(true);

			lock(this)
			{
				if (status != open)
					return null;

				long currentTime = SupportClass.currentTimeMillis();
				long stopTime = currentTime + timeout;
				System.Object o = null;
				
				while (true)
				{
                    if (status != open || (o = tryGet()) != null)
						return o;

					currentTime = SupportClass.currentTimeMillis();
					if (stopTime <= currentTime)
						throw new System.Threading.ThreadInterruptedException("Get operation timed out");
					
					try
					{
						//UPGRADE_TODO: threading!
                        System.Threading.Monitor.Wait(this, new TimeSpan((stopTime - currentTime)*TimeSpan.TicksPerMillisecond));
					}
					catch (System.Threading.ThreadInterruptedException)
					{
						// ignore, but really should retry operation instead
					}
				}
			}
		}
		
		// attempt to retrieve message from queue head
		public virtual System.Object tryGet()
		{
			System.Object o = null;
			
			if (head != null)
			{
				o = head.getContents();
				head = head.getNext();
				count--;
				
				if (head == null)
				{
					tail = null;
					count = 0;
				}
			}
			
			return o;
		}

		public virtual int getCount()
		{
			lock(this)
			{
				return count;
			}
		}
		
		/*
		* The Bucket class. The queue is implemented as a linked list
		* of Buckets. The container holds the queued object and a 
		* reference to the next Bucket. */
		internal class Bucket
		{
			private void  InitBlock(GenericQueue enclosingInstance)
			{
				this.enclosingInstance = enclosingInstance;
			}
			private GenericQueue enclosingInstance;
			private Bucket next;
			private System.Object contents;
			
			public Bucket(GenericQueue enclosingInstance, System.Object o)
			{
				InitBlock(enclosingInstance);
				next = null;
				contents = o;
			}
			
			public virtual void  setNext(Bucket newNext)
			{
				next = newNext;
			}
			
			public virtual Bucket getNext()
			{
				return next;
			}
			
			public virtual System.Object getContents()
			{
				return contents;
			}
		}
	}
}