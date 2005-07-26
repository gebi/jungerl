/*
---------------------------------------------------------------------------

 * 	This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2, or (at your option)
 *      any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with this program (see the file COPYING); if not, write to the
 *      Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *      Boston, MA  02111-1307, USA
 
 	This code is copyright 2003 by Matthew Eernisse
 
---------------------------------------------------------------------------	
*/ 

// The var docForm should be a reference to a <form>

function fd2qs(docForm) {      // FormData2QueryString
	var strSubmitContent = '';
	var formElem;
	var strLastElemName = '';
	
	for (i = 0; i < docForm.elements.length; i++) {
		
		formElem = docForm.elements[i];
		switch (formElem.type) {
			// Text fields, hidden form elements
			case 'text':
			case 'hidden':
			case 'password':
			case 'textarea':
			case 'select-one':
				strSubmitContent += formElem.name + '=' + escape(formElem.value) + '&'
				break;
				
			// Radio buttons
			case 'radio':
				if (formElem.checked) {
					strSubmitContent += formElem.name + '=' + escape(formElem.value) + '&'
				}
				break;
				
			// Checkboxes
			case 'checkbox':
				if (formElem.checked) {
					// Continuing multiple, same-name checkboxes
					if (formElem.name == strLastElemName) {
						// Strip of end ampersand if there is one
						if (strSubmitContent.lastIndexOf('&') == strSubmitContent.length-1) {
							strSubmitContent = strSubmitContent.substr(0, strSubmitContent.length - 1);
						}
						// Append value as comma-delimited string
						strSubmitContent += ',' + escape(formElem.value);
					}
					else {
						strSubmitContent += formElem.name + '=' + escape(formElem.value);
					}
					strSubmitContent += '&';
				}
				break;
				
		}
		strLastElemName = formElem.name
	}
	
	// Remove trailing separator
	strSubmitContent = strSubmitContent.substr(0, strSubmitContent.length - 1);
	return strSubmitContent;
}
