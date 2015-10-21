(*
  *                  Delphi Multi-tab Chromium Browser Frame
  *
  * Usage allowed under the restrictions of the Lesser GNU General Public License
  * or alternatively the restrictions of the Mozilla Public License 1.1
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  * the specific language governing rights and limitations under the License.
  *
  * Unit owner : BccSafe <bccsafe5988@gmail.com>
  * QQ         : 1262807955
  * Web site   : http://www.bccsafe.com
  * Repository : https://github.com/bccsafe/DcefBrowser
  *
  * The code of DcefBrowser is based on DCEF3 by: Henri Gourvest <hgourvest@gmail.com>
  * code: https://github.com/hgourvest/dcef3
  *
  * Embarcadero Technologies, Inc is not permitted to use or redistribute
  * this source code without explicit permission.
  *
*)

 {.$define CEF_STRING_TYPE_UTF8}
{$define CEF_STRING_TYPE_UTF16}
{.$define CEF_STRING_TYPE_WIDE}

{.$DEFINE CEF_MULTI_THREADED_MESSAGE_LOOP}

{$IFDEF FPC}
  // force multithreading message loop on FPC, still not work
  {$DEFINE CEF_MULTI_THREADED_MESSAGE_LOOP}
  {$DEFINE SUPPORTS_INLINE}
{$ENDIF}
{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

{$IF RTLVersion>=13}
  {$DEFINE DELPHI5_UP}
{$IFEND}

{$IF RTLVersion>=14}
  {$DEFINE DELPHI6_UP}
{$IFEND}

{$IF RTLVersion>=15}
  {$DEFINE DELPHI7_UP}
{$IFEND}

{$IF RTLVersion>=16}
  {$DEFINE DELPHI8_UP}
{$IFEND}

// Delphi 2005
{$IF RTLVersion>=17}
  {$DEFINE SUPPORTS_INLINE}
  {$DEFINE DELPHI9_UP}
{$IFEND}

{$IF RTLVersion>=18}
  {$DEFINE DELPHI10_UP}
{$IFEND}

{$IF RTLVersion>=18.5}
  {$DEFINE DELPHI11_UP}
{$IFEND}

{$IF RTLVersion>=20}
  {$DEFINE DELPHI12_UP}
{$IFEND}

{$IF RTLVersion>=21}
  {$DEFINE DELPHI14_UP}
{$IFEND}

{$IF RTLVersion>=22}
  {$DEFINE DELPHI15_UP}
{$IFEND}

{$IF RTLVersion>=23}
  {$DEFINE DELPHI16_UP}
{$IFEND}

{$IF RTLVersion>=24}
  {$DEFINE DELPHI17_UP}
{$IFEND}

{$IF RTLVersion>=25}
  {$DEFINE DELPHI18_UP}
{$IFEND}

{$IF RTLVersion>=26}
  {$DEFINE DELPHI19_UP}
{$IFEND}

{$IF RTLVersion>=27}
  {$DEFINE DELPHI20_UP}
{$IFEND}

{$IF RTLVersion>=28}
  {$DEFINE DELPHI21_UP}
{$IFEND}

{$IF RTLVersion>=29}
  {$DEFINE DELPHI22_UP}
{$IFEND}

{$IF RTLVersion>=300}
  {$DEFINE DELPHI23_UP}
{$IFEND}

