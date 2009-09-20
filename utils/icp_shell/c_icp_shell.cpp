
/***********************************************************************
 * Description: Contains programs for a convenient golog shell.
 *
 * last modified: $Date$
 *            by: $Author$
 *
 * $Id$
 **********************************************************************/

/*
 * icp_shell prolog implementation file by Carsten Gester & Stefan Jacobs
 * Copyright (C) 2004 KBSG, Aachen University <Stefan_J@gmx.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * Below are the color init strings for the basic file types. A color
 * init string consists of one or more of the following numeric codes:
 *
 * Attribute codes:
 * 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
 *
 * Text color codes:
 * 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan
 * 37=white
 *
 * Background color codes:
 * 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan
 * 47=white
 */

#define _KEY_ESC_ 27
#define _KEY_BACKSPACE_ 127
#define _KEY_RET_ 10
#define _KEY_TAB_ 9
#define _KEY_CTRL_D_ 4 

#include "c_icp_shell.h"

#include <sstream>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include "getkey.h"

// Return Minimum
template<class T> T Min(const T &a, const T &b)
{
  if(a<b)
    return a;
  return b;
}


std::vector<std::string> parseKeywordList(std::string keywordlist)
{
  std::vector<std::string> retVal;
  std::string buffer = "";

  bool parenthesis = false;
  int parameter_count = 0;
  
  for (unsigned int i = 0; i < keywordlist.length(); i++)
    {
       if ( keywordlist[i] == '(' ) 
         {
           parenthesis = true; // ignore everything in parenthesis
	   parameter_count=1;
	 }
       else if ( keywordlist[i] == ')' ) 
         {
           parenthesis = false; // stop ignoring
	 }
       else if ( ( parenthesis ) && ( keywordlist[i] == ',' ) )
         {
           parameter_count++;
         } 
       else if ( ( !parenthesis ) &&  
	         ( keywordlist[i] != '[' ) &&  
	         ( keywordlist[i] != ']' ) && 
		 ( keywordlist[i] != '\"' ) &&
		 ( keywordlist[i] != '\'' ) &&
	         ( keywordlist[i] != ' ' ) )
	 {
	   if ( keywordlist[i] == ',' )  // next word
             {
	       std::ostringstream a;
	       a << parameter_count;
	       buffer += " /" + a.str();
               retVal.push_back(buffer);
	       buffer = "";
	       parameter_count = 0;
	     }
           else
             buffer += keywordlist[i]; // buffer character
	 }  
       // else next character of keywordlist
    }

  std::ostringstream a;
  a << parameter_count;
  buffer += " /" + a.str();

  // push_back last word
  retVal.push_back(buffer);
  buffer = "";

  std::sort(retVal.begin(),retVal.end());

  return retVal;

}


bool is_prefix(std::string pref, std::string str)
{
  return  ( str.find(pref) == 0);
}

std::string find_longest_common_prefix(std::string common_prefix, std::vector<std::string> strVec)
{
  std::string retval = "";
  std::string common_pre ="";
  bool notallpre = true;

  for(unsigned i = 0; ((i < strVec.size()) &&  notallpre); i++) {
    common_pre = common_prefix + strVec[i][common_prefix.length()];
    if (strVec[i][common_prefix.length()] != ' ') {
      bool allpre = true;
      for(unsigned j = 0; ((j < strVec.size()) && allpre); j++) {
	allpre = allpre && is_prefix(common_pre, strVec[j]);
      }
      notallpre = !allpre;
    } 
  }

  if (notallpre) {
    retval = common_prefix;
  } else {
    retval = find_longest_common_prefix(common_pre,strVec);
  }

  return retval;

}


void find_possible_completations(std::string &prefix, std::vector<std::string> compVec)
{
  
  std::vector<std::string> posCompl;

  for(unsigned i = 0; i < compVec.size(); i++) {
    if( is_prefix(prefix,compVec[i]) ) {
      posCompl.push_back(compVec[i]);
    }
  }

  std::string newprefix = find_longest_common_prefix(prefix,posCompl);

  if (newprefix != prefix) {
    prefix = newprefix;
  } else {
    std::cout << std::endl << std::endl << std::flush;
    for(unsigned i = 0; i < posCompl.size(); i++) {
     std::cout << posCompl[i]<< std::endl << std::flush;
    }
    std::cout << std::endl << std::flush;
  }

}

void get_postfix(std::string str, std::string &prefix, std::string &postfix)
{

  prefix = "";
  postfix = "";

  unsigned long int last_space = str.find_last_of(" ");
  unsigned long int last_parenthesis_right = str.find_last_of(")");
  unsigned long int last_parenthesis_left = str.find_last_of("(");
  unsigned long int last_bracket = str.find_last_of("]");
  unsigned long int last_comma = str.find_last_of(",");
  unsigned long int last_greater = str.find_last_of(">");
  unsigned long int last_smaller = str.find_last_of("<");
  unsigned long int last_equal = str.find_last_of("=");
  
  unsigned int last_word_start = 0;

  if ( (last_space != std::string::npos) && 
       (last_space > last_word_start) )  {
    last_word_start = last_space;
  }

  if ( (last_parenthesis_right != std::string::npos) && 
       (last_parenthesis_right > last_word_start) ) {
    last_word_start = last_parenthesis_right;
  }

  if ( (last_parenthesis_left != std::string::npos) && 
       (last_parenthesis_left > last_word_start) ) {
    last_word_start = last_parenthesis_left;
  }

  if ( (last_bracket != std::string::npos) &&
       (last_bracket > last_word_start) ) {
    last_word_start = last_bracket;
  }

  if ( (last_comma != std::string::npos) &&
       (last_comma > last_word_start) ) {
    last_word_start = last_comma;
  }

  if ( (last_greater != std::string::npos) &&
       (last_greater > last_word_start) ) {
    last_word_start = last_greater;
  }

  if ( (last_smaller != std::string::npos) &&
       (last_smaller > last_word_start) ) {
    last_word_start = last_smaller;
  }

  if ( (last_equal != std::string::npos) &&
       (last_equal > last_word_start) ) {
    last_word_start = last_equal;
  }

  if (last_word_start > 0 ) {
    for (unsigned int i = 0; i <= last_word_start; i++) {
      prefix += str[i];
    }

    for (unsigned int i = last_word_start +1; i < str.length(); i++) {
	postfix += str[i];
    }
  } else {
    postfix = str;
  }

}


std::string read_line( std::vector<std::string> KeywordVec, std::vector<std::string> &HistoryVec, std::string Prompt = "")
{
  HistoryVec.push_back("");
  int history_pos = HistoryVec.size() -1;
  int cursor_pos = 0;
  unsigned int oldsize = 0;
  bool quit = false;
  char ch;
  
  std::cout << Prompt << std::flush;
  while (!quit) {

    ch = getkey(1);
    if (ch == _KEY_CTRL_D_) {

      // CTRL / STRG D pressed
      std::cout << std::endl << "Quiting!" << std::endl << std::flush;
      HistoryVec.push_back("quit");
      history_pos= (int)HistoryVec.size()-1;
      quit = true;
      
    } else if (ch == _KEY_RET_) {

      // return pressed
      if (history_pos != (int)HistoryVec.size() -1) {
	HistoryVec.push_back(HistoryVec[history_pos]);
	history_pos= (int)HistoryVec.size()-1;
      }
      std::cout << std::endl << "Executing:" << std::endl << std::flush;
      quit = true;

    } else  if (ch == _KEY_ESC_) {
      // ESC-Sequence
      char ch1 = getkey(1);
      if (ch1 == '[') {
          char ch2 = getkey(1);
          if (ch2 == 'A') {
            // UP
	    if ( history_pos > 0 ) {
              history_pos--;
	      cursor_pos = HistoryVec[history_pos].length();
            } else {
              std::cout << "\a" << std::flush;
	    }
          } else if (ch2 == 'B') {
            // DOWN
	    if ( history_pos < (int) HistoryVec.size() -1 ) {
              history_pos++;
	      cursor_pos = HistoryVec[history_pos].length();
            } else {
              std::cout << "\a" << std::flush;
	    }
          } else if (ch2 == 'C') {
            // RIGHT
            if ( cursor_pos < (int) HistoryVec[history_pos].length() ) {
              cursor_pos++;
            } else {
              std::cout << "\a" << std::flush;
	    }
          } else if (ch2 == 'D') {
            // LEFT
            if ( cursor_pos > 0) {
              cursor_pos--;
            } else {
              std::cout << "\a" << std::flush;
	    }
          } else if ((ch2 == 'F')|| ((ch2 == '4') && (getkey(1)=='~'))) {
            // END
            cursor_pos = (int)HistoryVec[history_pos].length();
          } else if ((ch2 == 'H') || ((ch2 == '1') && (getkey(1)=='~'))){
            // HOME
            cursor_pos=0;
          } else if ((ch2 == '3') && (getkey(1)=='~') ){
            // DEL
	    if  ( (cursor_pos >-1) && (cursor_pos < (int)HistoryVec[history_pos].length()) ) {
       
	      std::string buffer = "";
	      for (unsigned int i = 0; i < HistoryVec[history_pos].length(); i++) {
		if ( (int)i != cursor_pos )
		  buffer += HistoryVec[history_pos][i];
	      }   
	      if (history_pos != (int)HistoryVec.size()-1) {
		HistoryVec.push_back(buffer);
		history_pos= (int)HistoryVec.size()-1;
	      } else {
		HistoryVec[history_pos] = buffer;		
	      }

            } else std::cout << "\a" << std::flush;
            
	  } else { 
              std::cerr << std::endl << "Key is not defined (yet)!" <<std::endl;
	      do {
                usleep(1);
	      } while( getkey() != 0 );
	  }
	} else {
          std::cerr << std::endl << "Key is not defined (yet)!" <<std::endl;
	  do {
            usleep(1);
	  } while( getkey() != 0 );
	}

    } else if (ch == _KEY_TAB_) {
      // TAB 
      std::string prefix="";
      std::string postfix="";
      get_postfix(HistoryVec[history_pos],prefix,postfix);
      find_possible_completations(postfix,KeywordVec);
      if ((HistoryVec[history_pos] != prefix + postfix) && (history_pos != (int)HistoryVec.size()-1) ){
	HistoryVec.push_back(prefix + postfix);
	history_pos= (int)HistoryVec.size()-1;
      } else {
	HistoryVec[history_pos]=prefix + postfix;
      }
      cursor_pos = HistoryVec[history_pos].length();

    } else if (ch == _KEY_BACKSPACE_) {
       // backspace
      if (cursor_pos > 0) {
       
        std::string buffer = "";
        for (unsigned int i = 0; i < HistoryVec[history_pos].length() ; i++) {
          if ( (int)i != cursor_pos-1 )
            buffer += HistoryVec[history_pos][i];
	}
	if (history_pos != (int)HistoryVec.size()-1) {
	  HistoryVec.push_back(buffer);
	  history_pos= (int)HistoryVec.size()-1;
	} else {
	  HistoryVec[history_pos] = buffer;		
	}
	
        cursor_pos--;

      } else std::cout << "\a" << std::flush;
      
    } else if ( 31 < ch && ch <= 127) {
      // "normal" key pressed

      std::string buffer = "";
      if ( cursor_pos == (int) HistoryVec[history_pos].length() ) {
	buffer = HistoryVec[history_pos] + ch;
	if (history_pos != (int)HistoryVec.size()-1) {
	  HistoryVec.push_back(buffer);
	  history_pos= (int)HistoryVec.size()-1;
	} else {
	  HistoryVec[history_pos] = buffer;		
	}
      } else {
        
        for(unsigned int i = 0; i <= HistoryVec[history_pos].length(); i++) 
	  if( (int) i < cursor_pos) {
	    buffer += HistoryVec[history_pos][i]; 
          } else if( (int) i == cursor_pos) {
	    buffer += ch;
          } else { // (int) i > cursor_pos
            buffer += HistoryVec[history_pos][i-1];
	  }
	  
	if (history_pos != (int)HistoryVec.size()-1) {
	  HistoryVec.push_back(buffer);
	  history_pos= (int)HistoryVec.size()-1;
	} else {
	  HistoryVec[history_pos] = buffer;		
	}
      }
    
      cursor_pos++;

    } else {
      // undefined special key pressed
      std::cerr << std::endl << "Key sequenze: "<< (int) ch << std::flush;  
      do {
        ch = getkey();
	std::cerr << "," << (int) ch << std::flush;
        usleep(1);
      } while( ch != 0 );
      std::cerr << " is not defined!" << std::endl << std::flush;
    }

    if (HistoryVec[history_pos] == "") cursor_pos = 0;

    if (!quit) {

      std::cout << "\r" << Prompt  << std::flush;
      for (unsigned int i = 0; i < oldsize +3; i++) {
	std::cout << " "<< std::flush;
      }
 
      unsigned int green_start = HistoryVec[history_pos].size()+10; // will not be printed anymore
      unsigned int green_stop = HistoryVec[history_pos].size()+10; // will not be printed anymore
  
      if (cursor_pos <= (int) HistoryVec[history_pos].size()) {
        std::string buffer = HistoryVec[history_pos];
        if( (cursor_pos < (int) HistoryVec[history_pos].size()) && (buffer[cursor_pos] == '(')) {
	  green_start = cursor_pos;
          bool found = false;
          int open_count = 1;
	  for (  unsigned int i = green_start+1; (i < buffer.size()) && (! found); i++) {
	    if (buffer[i] == '(') open_count++;
	    if (buffer[i] == ')') open_count--;
	    
	    if (open_count == 0) {
	      found = true;
	      green_stop  = i;
	    }
	  }
          if(!found) {
	    green_start = HistoryVec[history_pos].size()+10; // will not be printed anymore
            green_stop = HistoryVec[history_pos].size() +10; // will not be printed anymore
	  }
          
	} else if ( (cursor_pos < (int) HistoryVec[history_pos].size()) && (buffer[cursor_pos] == '[')) {
	  green_start = cursor_pos;
          bool found = false;
          int open_count = 1;
	  for (  unsigned int i = green_start+1; (i < buffer.size()) && (! found); i++) {
	    if (buffer[i] == '[') open_count++;
	    if (buffer[i] == ']') open_count--;
	    if (open_count == 0) {
	      found = true;
	      green_stop  = i;
	    } 
	    
	  }
          if(!found) {
	    green_start = HistoryVec[history_pos].size()+10; // will not be printed anymore
            green_stop = HistoryVec[history_pos].size() +10; // will not be printed anymore
	  }
      
	} else if ((cursor_pos>0) && (buffer[cursor_pos-1] == ')')) { 
          green_stop = cursor_pos-1;
          bool found = false;
          int open_count = 1;
          for (  unsigned int i = green_stop-1; (i >=0) && (! found); i--) {
	    if (buffer[i] == ')') open_count++;
	    if (buffer[i] == '(') open_count--;
	    if (open_count == 0) {
	      found = true;
	      green_start  = i;
	    } 
	    
	  }
          if(!found) {
	    green_start = HistoryVec[history_pos].size()+10; // will not be printed anymore
            green_stop = HistoryVec[history_pos].size() +10; // will not be printed anymore
	  }

	} else if ((cursor_pos>0) && (buffer[cursor_pos-1] == ']')) {

          green_stop = cursor_pos-1;
          bool found = false;
          int open_count = 1;
          for (  unsigned int i = green_stop-1; (i >=0) && (! found); i--) {
	    if (buffer[i] == ']') open_count++;
	    if (buffer[i] == '[') open_count--;
	    if (open_count == 0) {
	      found = true;
	      green_start  = i;
	    } 
	    
	  }
          if(!found) {
	    green_start = HistoryVec[history_pos].size()+10; // will not be printed anymore
            green_stop = HistoryVec[history_pos].size() +10; // will not be printed anymore
	  }

	} else {
	  green_start = HistoryVec[history_pos].size()+10; // will not be printed anymore
          green_stop = HistoryVec[history_pos].size() +10; // will not be printed anymore
	}
      }

      std::cout << "\r\033[0m" << Prompt << std::flush;
      for (unsigned int i = 0; i < HistoryVec[history_pos].size()+3; i++) {

	std::string normal_textcolor =  "\033[0m";
        std::string cursor_color = "\033[07m\033[1;07m";

        if ( (green_start == i) || (i == green_stop ) ) {
          normal_textcolor = "\033[1;37m\033[0;42m";
          cursor_color = "\033[1;37m\033[0;45m";
        } else if ( (green_start < i) && (i < green_stop ) ){
          normal_textcolor = "\033[0m\033[0;32m";
          cursor_color = "\033[1;32m\033[0;42m";
	} else {
	  normal_textcolor =  "\033[0m";
          cursor_color = "\033[0;07m";
	}

        if ((int)i ==  cursor_pos) {
    	  if (i < HistoryVec[history_pos].size() ) {
	    std::cout << cursor_color << HistoryVec[history_pos][i]  << std::flush;
	  } else {
            std::cout << cursor_color << " " << std::flush;
	  }
	} else {
          if (i < HistoryVec[history_pos].size() ) {
            std::cout << normal_textcolor << HistoryVec[history_pos][i] << std::flush;
	  } else {
           std::cout << normal_textcolor <<" " << std::flush;
          }
	}
      }
      std::cout << "\033[0m " << std::flush;
      oldsize = HistoryVec[history_pos].length();
    }


  } 
 
  return HistoryVec[history_pos];
}

std::string c_icp_shell(std::string Prompt, std::string KeywordList) 
{

  std::string buffer;
  std::vector<std::string> HistoryList;
  std::string historyfilename = std::string(getenv("HOME"));
  historyfilename += "/.icp_history";
  std::ifstream iHistoryFile(historyfilename.c_str());
  
  if( !(!iHistoryFile) ) { // file exists and is readable
    while (!iHistoryFile.eof()) {
      std::getline(iHistoryFile, buffer);
      if ((buffer != "quit")&& (buffer != "" )) {
	HistoryList.push_back(buffer);
      }
    }
    iHistoryFile.close();  
  }

  SetupBreakHandler();

  system("setterm -cursor off");

  std::string retval =  read_line(parseKeywordList(KeywordList),HistoryList,Prompt);

  std::cout << "\033[0m " << std::flush;

  system("setterm -cursor on");

  RetrackBreakHandler();
  
  std::ofstream oHistoryFile(historyfilename.c_str());
  if( !(!oHistoryFile) ) { // file exists and is writeable
    for( unsigned int i = 0; 
	 i < Min((unsigned int)HistoryList.size(),(unsigned int)1024); 
	 i++ ) {
      unsigned int j = HistoryList.size() - Min((unsigned int)HistoryList.size(),(unsigned int)1024) + i;
      if ((HistoryList[j] != "quit" ) && (HistoryList[j] != "" )) {
	for (unsigned int k = 0; k < HistoryList[j].size(); k++) {
	  if ((31 < (unsigned int)HistoryList[j][k]) && 
	      ((unsigned int)HistoryList[j][k] < 128)) {
	    oHistoryFile << HistoryList[j][k];
	  }
	}
	oHistoryFile << std::endl;
      }
    }
    oHistoryFile.close();
  }

  return retval;
}
