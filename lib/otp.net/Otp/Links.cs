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
	// package scope
	using System;

	public class Links
	{
		internal Link[] _links;
		internal int _count;
		
		internal Links():this(10)
		{
		}
		
		internal Links(int initialSize)
		{
			_links = new Link[initialSize];
			_count = 0;
		}

		internal virtual void  addLink(Erlang.Pid local, Erlang.Pid remote)
		{
			lock(this)
			{
				int i;
				
				if ((i = find(local, remote)) == - 1)
				{
					if (_count >= _links.Length)
					{
						Link[] tmp = new Link[_count * 2];
						Array.Copy(_links, 0, tmp, 0, _count);
						_links = tmp;
					}
					_links[_count++] = new Link(local, remote);
				}
			}
		}

		internal virtual void  removeLink(Erlang.Pid local, Erlang.Pid remote)
		{
			lock(this)
			{
				int i;
				
				if ((i = find(local, remote)) != - 1)
				{
					_count--;
					_links[i] = _links[_count];
					_links[_count] = null;
				}
			}
		}

		internal virtual bool exists(Erlang.Pid local, Erlang.Pid remote)
		{
			lock(this)
			{
				return (find(local, remote) != - 1);
			}
		}

		internal virtual int find(Erlang.Pid local, Erlang.Pid remote)
		{
			lock(this)
			{
				 for (int i = 0; i < _count; i++)
				{
					if (_links[i].equals(local, remote))
						return i;
				}
				return - 1;
			}
		}
		
		internal virtual int count()
		{
			return _count;
		}
		
		/*all local pids get notified about broken connection */
		internal virtual Erlang.Pid[] localPids()
		{
			lock(this)
			{
				Erlang.Pid[] ret = null;
				if (_count != 0)
				{
					ret = new Erlang.Pid[_count];
					 for (int i = 0; i < _count; i++)
					{
						ret[i] = _links[i].local();
					}
				}
				return ret;
			}
		}
		
		/*all remote pids get notified about failed pid */
		internal virtual Erlang.Pid[] remotePids()
		{
			lock(this)
			{
				Erlang.Pid[] ret = null;
				if (_count != 0)
				{
					ret = new Erlang.Pid[_count];
					 for (int i = 0; i < _count; i++)
					{
						ret[i] = _links[i].remote();
					}
				}
				return ret;
			}
		}
		
		/*clears the link table, returns a copy */
		internal virtual Link[] clearLinks()
		{
			lock(this)
			{
				Link[] ret = null;
				if (_count != 0)
				{
					ret = new Link[_count];
					 for (int i = 0; i < _count; i++)
					{
						ret[i] = _links[i];
						_links[i] = null;
					}
					_count = 0;
				}
				return ret;
			}
		}
		
		/*returns a copy of the link table */
		internal virtual Link[] links()
		{
			lock(this)
			{
				Link[] ret = null;
				if (_count != 0)
				{
					ret = new Link[_count];
					Array.Copy(_links, 0, ret, 0, _count);
				}
				return ret;
			}
		}
	}
}