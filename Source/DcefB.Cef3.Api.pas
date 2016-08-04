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

unit DcefB.Cef3.Api;

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}
{$I DcefB.Dcef3.cef.inc}

interface

uses
  DcefB.Cef3.Types;

type
  PCefv8Handler = ^TCefv8Handler;
  PCefV8Accessor = ^TCefV8Accessor;
  PCefv8Value = ^TCefv8Value;
  PCefV8StackTrace = ^TCefV8StackTrace;
  PCefV8StackFrame = ^TCefV8StackFrame;
  PCefV8ValueArray = array [0 .. (High(Integer) div SizeOf(Pointer)) - 1]
    of PCefv8Value;
  PPCefV8Value = ^PCefV8ValueArray;
  PCefSchemeHandlerFactory = ^TCefSchemeHandlerFactory;
  PCefSchemeRegistrar = ^TCefSchemeRegistrar;
  PCefFrame = ^TCefFrame;
  PCefRequest = ^TCefRequest;
  PCefStreamReader = ^TCefStreamReader;
  PCefPostData = ^TCefPostData;
  PCefPostDataElement = ^TCefPostDataElement;
  PPCefPostDataElement = ^PCefPostDataElement;
  PCefReadHandler = ^TCefReadHandler;
  PCefWriteHandler = ^TCefWriteHandler;
  PCefStreamWriter = ^TCefStreamWriter;
  PCefBase = ^TCefBase;
  PCefBrowser = ^TCefBrowser;
  PCefRunFileDialogCallback = ^TCefRunFileDialogCallback;
  PCefBrowserHost = ^TCefBrowserHost;
  PCefPdfPrintCallback = ^TCefPdfPrintCallback;
  PCefTask = ^TCefTask;
  PCefTaskRunner = ^TCefTaskRunner;
  PCefDownloadHandler = ^TCefDownloadHandler;
  PCefXmlReader = ^TCefXmlReader;
  PCefZipReader = ^TCefZipReader;
  PCefDomVisitor = ^TCefDomVisitor;
  PCefDomDocument = ^TCefDomDocument;
  PCefDomNode = ^TCefDomNode;
  PCefResponse = ^TCefResponse;
  PCefv8Context = ^TCefv8Context;
  PCefCookieVisitor = ^TCefCookieVisitor;
  PCefCookie = ^TCefCookie;
  PCefClient = ^TCefClient;
  PCefLifeSpanHandler = ^TCefLifeSpanHandler;
  PCefLoadHandler = ^TCefLoadHandler;
  PCefRequestHandler = ^TCefRequestHandler;
  PCefDisplayHandler = ^TCefDisplayHandler;
  PCefFocusHandler = ^TCefFocusHandler;
  PCefKeyboardHandler = ^TCefKeyboardHandler;
  PCefJsDialogHandler = ^TCefJsDialogHandler;
  PCefApp = ^TCefApp;
  PCefV8Exception = ^TCefV8Exception;
  PCefResourceBundleHandler = ^TCefResourceBundleHandler;
  PCefCookieManager = ^TCefCookieManager;
  PCefWebPluginInfo = ^TCefWebPluginInfo;
  PCefCommandLine = ^TCefCommandLine;
  PCefProcessMessage = ^TCefProcessMessage;
  PCefBinaryValue = ^TCefBinaryValue;
  PPCefBinaryValue = ^PCefBinaryValue;
  PCefDictionaryValue = ^TCefDictionaryValue;
  PCefListValue = ^TCefListValue;
  PCefBrowserProcessHandler = ^TCefBrowserProcessHandler;
  PCefRenderProcessHandler = ^TCefRenderProcessHandler;
  PCefAuthCallback = ^TCefAuthCallback;
  PCefRequestCallback = ^TCefRequestCallback;
  PCefResourceHandler = ^TCefResourceHandler;
  PCefCallback = ^TCefCallback;
  PCefCompletionCallback = ^TCefCompletionCallback;
  PCefRunContextMenuCallback = ^TCefRunContextMenuCallback;
  PCefContextMenuHandler = ^TCefContextMenuHandler;
  PCefContextMenuParams = ^TCefContextMenuParams;
  PCefMenuModel = ^TCefMenuModel;
  PCefGeolocationCallback = ^TCefGeolocationCallback;
  PCefGeolocationHandler = ^TCefGeolocationHandler;
  PCefBeforeDownloadCallback = ^TCefBeforeDownloadCallback;
  PCefDownloadItemCallback = ^TCefDownloadItemCallback;
  PCefDownloadItem = ^TCefDownloadItem;
  PCefStringVisitor = ^TCefStringVisitor;
  PCefJsDialogCallback = ^TCefJsDialogCallback;
  PCefUrlRequest = ^TCefUrlRequest;
  PCefUrlRequestClient = ^TCefUrlRequestClient;
  PCefWebPluginInfoVisitor = ^TCefWebPluginInfoVisitor;
  PCefWebPluginUnstableCallback = ^TCefWebPluginUnstableCallback;
  PCefFileDialogCallback = ^TCefFileDialogCallback;
  PCefDialogHandler = ^TCefDialogHandler;
  PCefRenderHandler = ^TCefRenderHandler;
  PCefGetGeolocationCallback = ^TCefGetGeolocationCallback;
  PCefEndTracingCallback = ^TCefEndTracingCallback;
  PCefScreenInfo = ^TCefScreenInfo;
  PCefDragData = ^TCefDragData;
  PCefDragHandler = ^TCefDragHandler;
  PCefRequestContextHandler = ^TCefRequestContextHandler;
  PCefRequestContext = ^TCefRequestContext;
  PCefPrintSettings = ^TCefPrintSettings;
  PCefPrintDialogCallback = ^TCefPrintDialogCallback;
  PCefPrintJobCallback = ^TCefPrintJobCallback;
  PCefPrintHandler = ^TCefPrintHandler;
  PCefNavigationEntry = ^TCefNavigationEntry;
  PCefNavigationEntryVisitor = ^TCefNavigationEntryVisitor;
  PCefFindHandler = ^TCefFindHandler;
  PCefSetCookieCallback = ^TCefSetCookieCallback;
  PCefDeleteCookiesCallback = ^TCefDeleteCookiesCallback;
  PCefValue = ^TCefValue;
  PCefSslCertPrincipal = ^TCefSslCertPrincipal;
  PCefSslInfo = ^TCefSslInfo;
  PCefResourceBundle = ^TCefResourceBundle;
  PCefResponseFilter = ^TCefResponseFilter;

  // Structure defining the reference count implementation functions. All
  // framework structures must include the cef_base_t structure first.
  TCefBase = record
    // Size of the data structure.
    size: NativeUInt;

    // Called to increment the reference count for the object. Should be called
    // for every new copy of a pointer to a given object.
    add_ref: procedure(self: PCefBase); stdcall;

    // Called to decrement the reference count for the object. If the reference
    // count falls to 0 the object should self-delete. Returns true (1) if the
    // resulting reference count is 0.
    release: function(self: PCefBase): Integer; stdcall;

    // Returns true (1) if the current reference count is 1.
    has_one_ref: function(self: PCefBase): Integer; stdcall;
  end;

  // Structure that wraps other data value types. Complex types (binary,
  // dictionary and list) will be referenced but not owned by this object. Can be
  // used on any process and thread.
  TCefValue = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if the underlying data is valid. This will always be true
    // (1) for simple types. For complex types (binary, dictionary and list) the
    // underlying data may become invalid if owned by another object (e.g. list or
    // dictionary) and that other object is then modified or destroyed. This value
    // object can be re-used by calling Set*() even if the underlying data is
    // invalid.

    is_valid: function(self: PCefValue): Integer; stdcall;

    // Returns true (1) if the underlying data is owned by another object.
    is_owned: function(self: PCefValue): Integer; stdcall;

    // Returns true (1) if the underlying data is read-only. Some APIs may expose
    // read-only objects.
    is_read_only: function(self: PCefValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have the same underlying
    // data. If true (1) modifications to this object will also affect |that|
    // object and vice-versa.
    is_same: function(self, that: PCefValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have an equivalent
    // underlying value but are not necessarily the same object.
    is_equal: function(self, that: PCefValue): Integer; stdcall;

    // Returns a copy of this object. The underlying data will also be copied.
    copy: function(self: PCefValue): PCefValue; stdcall;

    // Returns the underlying value type.
    get_type: function(self: PCefValue): TCefValueType; stdcall;

    // Returns the underlying value as type bool.
    get_bool: function(self: PCefValue): Integer; stdcall;

    // Returns the underlying value as type int.
    get_int: function(self: PCefValue): Integer; stdcall;

    // Returns the underlying value as type double.
    get_double: function(self: PCefValue): Double; stdcall;

    // Returns the underlying value as type string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_string: function(self: PCefValue): PCefStringUserFree; stdcall;

    // Returns the underlying value as type binary. The returned reference may
    // become invalid if the value is owned by another object or if ownership is
    // transferred to another object in the future. To maintain a reference to the
    // value after assigning ownership to a dictionary or list pass this object to
    // the set_value() function instead of passing the returned reference to
    // set_binary().
    get_binary: function(self: PCefValue): PCefBinaryValue; stdcall;

    // Returns the underlying value as type dictionary. The returned reference may
    // become invalid if the value is owned by another object or if ownership is
    // transferred to another object in the future. To maintain a reference to the
    // value after assigning ownership to a dictionary or list pass this object to
    // the set_value() function instead of passing the returned reference to
    // set_dictionary().
    get_dictionary: function(self: PCefValue): PCefDictionaryValue; stdcall;

    // Returns the underlying value as type list. The returned reference may
    // become invalid if the value is owned by another object or if ownership is
    // transferred to another object in the future. To maintain a reference to the
    // value after assigning ownership to a dictionary or list pass this object to
    // the set_value() function instead of passing the returned reference to
    // set_list().
    get_list: function(self: PCefValue): PCefListValue; stdcall;

    // Sets the underlying value as type null. Returns true (1) if the value was
    // set successfully.
    set_null: function(self: PCefValue): Integer; stdcall;

    // Sets the underlying value as type bool. Returns true (1) if the value was
    // set successfully.
    set_bool: function(self: PCefValue; value: Integer): Integer; stdcall;

    // Sets the underlying value as type int. Returns true (1) if the value was
    // set successfully.
    set_int: function(self: PCefValue; value: Integer): Integer; stdcall;

    // Sets the underlying value as type double. Returns true (1) if the value was
    // set successfully.
    set_double: function(self: PCefValue; value: Double): Integer; stdcall;

    // Sets the underlying value as type string. Returns true (1) if the value was
    // set successfully.
    set_string: function(self: PCefValue; const value: PCefString): Integer; stdcall;

    // Sets the underlying value as type binary. Returns true (1) if the value was
    // set successfully. This object keeps a reference to |value| and ownership of
    // the underlying data remains unchanged.
    set_binary: function(self: PCefValue; value: PCefBinaryValue): Integer; stdcall;

    // Sets the underlying value as type dict. Returns true (1) if the value was
    // set successfully. This object keeps a reference to |value| and ownership of
    // the underlying data remains unchanged.
    set_dictionary: function(self: PCefValue; value: PCefDictionaryValue): Integer; stdcall;

    // Sets the underlying value as type list. Returns true (1) if the value was
    // set successfully. This object keeps a reference to |value| and ownership of
    // the underlying data remains unchanged.
    set_list: function(self: PCefValue; value: PCefListValue): Integer; stdcall;
  end;

  // Structure representing a binary value. Can be used on any process and thread.
  TCefBinaryValue = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. This object may become invalid if
    // the underlying data is owned by another object (e.g. list or dictionary)
    // and that other object is then modified or destroyed. Do not call any other
    // functions if this function returns false (0).
    is_valid: function(self: PCefBinaryValue): Integer; stdcall;

    // Returns true (1) if this object is currently owned by another object.
    is_owned: function(self: PCefBinaryValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have the same underlying
    // data.
    is_same: function(self, that: PCefBinaryValue):Integer; stdcall;

    // Returns true (1) if this object and |that| object have an equivalent
    // underlying value but are not necessarily the same object.
    is_equal: function(self, that: PCefBinaryValue): Integer; stdcall;

    // Returns a copy of this object. The data in this object will also be copied.
    copy: function(self: PCefBinaryValue): PCefBinaryValue; stdcall;

    // Returns the data size.
    get_size: function(self: PCefBinaryValue): NativeUInt; stdcall;

    // Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
    // the specified byte |data_offset|. Returns the number of bytes read.
    get_data: function(self: PCefBinaryValue; buffer: Pointer;
      buffer_size, data_offset: NativeUInt): NativeUInt; stdcall;
  end;

  // Structure representing a dictionary value. Can be used on any process and
  // thread.
  TCefDictionaryValue = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. This object may become invalid if
    // the underlying data is owned by another object (e.g. list or dictionary)
    // and that other object is then modified or destroyed. Do not call any other
    // functions if this function returns false (0).
    is_valid: function(self: PCefDictionaryValue): Integer; stdcall;

    // Returns true (1) if this object is currently owned by another object.
    is_owned: function(self: PCefDictionaryValue): Integer; stdcall;

    // Returns true (1) if the values of this object are read-only. Some APIs may
    // expose read-only objects.
    is_read_only: function(self: PCefDictionaryValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have the same underlying
    // data. If true (1) modifications to this object will also affect |that|
    // object and vice-versa.
    is_same: function(self, that: PCefDictionaryValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have an equivalent
    // underlying value but are not necessarily the same object.
    is_equal: function(self, that: PCefDictionaryValue): Integer; stdcall;

    // Returns a writable copy of this object. If |exclude_NULL_children| is true
    // (1) any NULL dictionaries or lists will be excluded from the copy.
    copy: function(self: PCefDictionaryValue; exclude_empty_children: Integer)
      : PCefDictionaryValue; stdcall;

    // Returns the number of values.
    get_size: function(self: PCefDictionaryValue): NativeUInt; stdcall;

    // Removes all values. Returns true (1) on success.
    clear: function(self: PCefDictionaryValue): Integer; stdcall;

    // Returns true (1) if the current dictionary has a value for the given key.
    has_key: function(self: PCefDictionaryValue; const key: PCefString)
      : Integer; stdcall;

    // Reads all keys for this dictionary into the specified vector.
    get_keys: function(self: PCefDictionaryValue; const keys: TCefStringList)
      : Integer; stdcall;

    // Removes the value at the specified key. Returns true (1) is the value was
    // removed successfully.
    remove: function(self: PCefDictionaryValue; const key: PCefString)
      : Integer; stdcall;

    // Returns the value type for the specified key.
    get_type: function(self: PCefDictionaryValue; const key: PCefString)
      : TCefValueType; stdcall;

    // Returns the value at the specified key. For simple types the returned value
    // will copy existing data and modifications to the value will not modify this
    // object. For complex types (binary, dictionary and list) the returned value
    // will reference existing data and modifications to the value will modify
    // this object.
    get_value: function(self: PCefDictionaryValue; const key: PCefString): PCefValue; stdcall;

    // Returns the value at the specified key as type bool.
    get_bool: function(self: PCefDictionaryValue; const key: PCefString)
      : Integer; stdcall;

    // Returns the value at the specified key as type int.
    get_int: function(self: PCefDictionaryValue; const key: PCefString)
      : Integer; stdcall;

    // Returns the value at the specified key as type double.
    get_double: function(self: PCefDictionaryValue; const key: PCefString)
      : Double; stdcall;

    // Returns the value at the specified key as type string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_string: function(self: PCefDictionaryValue; const key: PCefString)
      : PCefStringUserFree; stdcall;

    // Returns the value at the specified key as type binary. The returned value
    // will reference existing data.
    get_binary: function(self: PCefDictionaryValue; const key: PCefString): PCefBinaryValue; stdcall;

    // Returns the value at the specified key as type dictionary. The returned
    // value will reference existing data and modifications to the value will
    // modify this object.
    get_dictionary: function(self: PCefDictionaryValue; const key: PCefString): PCefDictionaryValue; stdcall;

    // Returns the value at the specified key as type list. The returned value
    // will reference existing data and modifications to the value will modify
    // this object.
    get_list: function(self: PCefDictionaryValue; const key: PCefString): PCefListValue; stdcall;

    // Sets the value at the specified key. Returns true (1) if the value was set
    // successfully. If |value| represents simple data then the underlying data
    // will be copied and modifications to |value| will not modify this object. If
    // |value| represents complex data (binary, dictionary or list) then the
    // underlying data will be referenced and modifications to |value| will modify
    // this object.
    set_value: function(self: PCefDictionaryValue; const key: PCefString; value: PCefValue): Integer; stdcall;

    // Sets the value at the specified key as type null. Returns true (1) if the
    // value was set successfully.
    set_null: function(self: PCefDictionaryValue; const key: PCefString): Integer; stdcall;

    // Sets the value at the specified key as type bool. Returns true (1) if the
    // value was set successfully.
    set_bool: function(self: PCefDictionaryValue; const key: PCefString;
      value: Integer): Integer; stdcall;

    // Sets the value at the specified key as type int. Returns true (1) if the
    // value was set successfully.
    set_int: function(self: PCefDictionaryValue; const key: PCefString;
      value: Integer): Integer; stdcall;

    // Sets the value at the specified key as type double. Returns true (1) if the
    // value was set successfully.
    set_double: function(self: PCefDictionaryValue; const key: PCefString;
      value: Double): Integer; stdcall;

    // Sets the value at the specified key as type string. Returns true (1) if the
    // value was set successfully.
    set_string: function(self: PCefDictionaryValue; const key: PCefString;
      value: PCefString): Integer; stdcall;

    // Sets the value at the specified key as type binary. Returns true (1) if the
    // value was set successfully. If |value| is currently owned by another object
    // then the value will be copied and the |value| reference will not change.
    // Otherwise, ownership will be transferred to this object and the |value|
    // reference will be invalidated.
    set_binary: function(self: PCefDictionaryValue; const key: PCefString;
      value: PCefBinaryValue): Integer; stdcall;

    // Sets the value at the specified key as type dict. Returns true (1) if the
    // value was set successfully. If |value| is currently owned by another object
    // then the value will be copied and the |value| reference will not change.
    // Otherwise, ownership will be transferred to this object and the |value|
    // reference will be invalidated.
    set_dictionary: function(self: PCefDictionaryValue; const key: PCefString;
      value: PCefDictionaryValue): Integer; stdcall;

    // Sets the value at the specified key as type list. Returns true (1) if the
    // value was set successfully. If |value| is currently owned by another object
    // then the value will be copied and the |value| reference will not change.
    // Otherwise, ownership will be transferred to this object and the |value|
    // reference will be invalidated.
    set_list: function(self: PCefDictionaryValue; const key: PCefString;
      value: PCefListValue): Integer; stdcall;
  end;

  // Structure representing a list value. Can be used on any process and thread.
  TCefListValue = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. This object may become invalid if
    // the underlying data is owned by another object (e.g. list or dictionary)
    // and that other object is then modified or destroyed. Do not call any other
    // functions if this function returns false (0).
    is_valid: function(self: PCefListValue): Integer; stdcall;

    // Returns true (1) if this object is currently owned by another object.
    is_owned: function(self: PCefListValue): Integer; stdcall;

    // Returns true (1) if the values of this object are read-only. Some APIs may
    // expose read-only objects.
    is_read_only: function(self: PCefListValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have the same underlying
    // data. If true (1) modifications to this object will also affect |that|
    // object and vice-versa.
    is_same: function(self, that: PCefListValue): Integer; stdcall;

    // Returns true (1) if this object and |that| object have an equivalent
    // underlying value but are not necessarily the same object.
    is_equal: function(self, that: PCefListValue): Integer; stdcall;

    // Returns a writable copy of this object.
    copy: function(self: PCefListValue): PCefListValue; stdcall;

    // Sets the number of values. If the number of values is expanded all new
    // value slots will default to type null. Returns true (1) on success.
    set_size: function(self: PCefListValue; size: NativeUInt): Integer; stdcall;

    // Returns the number of values.
    get_size: function(self: PCefListValue): NativeUInt; stdcall;

    // Removes all values. Returns true (1) on success.
    clear: function(self: PCefListValue): Integer; stdcall;

    // Removes the value at the specified index.
    remove: function(self: PCefListValue; index: Integer): Integer; stdcall;

    // Returns the value type at the specified index.
    get_type: function(self: PCefListValue; index: Integer)
      : TCefValueType; stdcall;

    // Returns the value at the specified index. For simple types the returned
    // value will copy existing data and modifications to the value will not
    // modify this object. For complex types (binary, dictionary and list) the
    // returned value will reference existing data and modifications to the value
    // will modify this object.
    get_value: function(self: PCefListValue; index: Integer): PCefValue; stdcall;

    // Returns the value at the specified index as type bool.
    get_bool: function(self: PCefListValue; index: Integer): Integer; stdcall;

    // Returns the value at the specified index as type int.
    get_int: function(self: PCefListValue; index: Integer): Integer; stdcall;

    // Returns the value at the specified index as type double.
    get_double: function(self: PCefListValue; index: Integer): Double; stdcall;

    // Returns the value at the specified index as type string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_string: function(self: PCefListValue; index: Integer)
      : PCefStringUserFree; stdcall;

    // Returns the value at the specified index as type binary. The returned value
    // will reference existing data.
    get_binary: function(self: PCefListValue; index: Integer): PCefBinaryValue; stdcall;

    // Returns the value at the specified index as type dictionary. The returned
    // value will reference existing data and modifications to the value will
    // modify this object.
    get_dictionary: function(self: PCefListValue; index: Integer): PCefDictionaryValue; stdcall;

    // Returns the value at the specified index as type list. The returned value
    // will reference existing data and modifications to the value will modify
    // this object.
    get_list: function(self: PCefListValue; index: Integer): PCefListValue; stdcall;

    // Sets the value at the specified index. Returns true (1) if the value was
    // set successfully. If |value| represents simple data then the underlying
    // data will be copied and modifications to |value| will not modify this
    // object. If |value| represents complex data (binary, dictionary or list)
    // then the underlying data will be referenced and modifications to |value|
    // will modify this object.
    set_value: function(self: PCefListValue; index: Integer; value: PCefValue): Integer; stdcall;

    // Sets the value at the specified index as type null. Returns true (1) if the
    // value was set successfully.
    set_null: function(self: PCefListValue; index: Integer): Integer; stdcall;

    // Sets the value at the specified index as type bool. Returns true (1) if the
    // value was set successfully.
    set_bool: function(self: PCefListValue; index, value: Integer)
      : Integer; stdcall;

    // Sets the value at the specified index as type int. Returns true (1) if the
    // value was set successfully.
    set_int: function(self: PCefListValue; index, value: Integer)
      : Integer; stdcall;

    // Sets the value at the specified index as type double. Returns true (1) if
    // the value was set successfully.
    set_double: function(self: PCefListValue; index: Integer; value: Double)
      : Integer; stdcall;

    // Sets the value at the specified index as type string. Returns true (1) if
    // the value was set successfully.
    set_string: function(self: PCefListValue; index: Integer; value: PCefString)
      : Integer; stdcall;

    // Sets the value at the specified index as type binary. Returns true (1) if
    // the value was set successfully. If |value| is currently owned by another
    // object then the value will be copied and the |value| reference will not
    // change. Otherwise, ownership will be transferred to this object and the
    // |value| reference will be invalidated.
    set_binary: function(self: PCefListValue; index: Integer;
      value: PCefBinaryValue): Integer; stdcall;

    // Sets the value at the specified index as type dict. Returns true (1) if the
    // value was set successfully. If |value| is currently owned by another object
    // then the value will be copied and the |value| reference will not change.
    // Otherwise, ownership will be transferred to this object and the |value|
    // reference will be invalidated.
    set_dictionary: function(self: PCefListValue; index: Integer;
      value: PCefDictionaryValue): Integer; stdcall;

    // Sets the value at the specified index as type list. Returns true (1) if the
    // value was set successfully. If |value| is currently owned by another object
    // then the value will be copied and the |value| reference will not change.
    // Otherwise, ownership will be transferred to this object and the |value|
    // reference will be invalidated.
    set_list: function(self: PCefListValue; index: Integer;
      value: PCefListValue): Integer; stdcall;
  end;

  // Implement this structure for asynchronous task execution. If the task is
  // posted successfully and if the associated message loop is still running then
  // the execute() function will be called on the target thread. If the task fails
  // to post then the task object may be destroyed on the source thread instead of
  // the target thread. For this reason be cautious when performing work in the
  // task object destructor.

  TCefTask = record
    // Base structure.
    base: TCefBase;
    // Method that will be executed on the target thread.
    execute: procedure(self: PCefTask); stdcall;
  end;

  // Structure that asynchronously executes tasks on the associated thread. It is
  // safe to call the functions of this structure on any thread.
  //
  // CEF maintains multiple internal threads that are used for handling different
  // types of tasks in different processes. The cef_thread_id_t definitions in
  // cef_types.h list the common CEF threads. Task runners are also available for
  // other CEF threads as appropriate (for example, V8 WebWorker threads).
  TCefTaskRunner = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is pointing to the same task runner as
    // |that| object.
    is_same: function(self, that: PCefTaskRunner): Integer; stdcall;

    // Returns true (1) if this task runner belongs to the current thread.
    belongs_to_current_thread: function(self: PCefTaskRunner): Integer; stdcall;

    // Returns true (1) if this task runner is for the specified CEF thread.
    belongs_to_thread: function(self: PCefTaskRunner; threadId: TCefThreadId)
      : Integer; stdcall;

    // Post a task for execution on the thread associated with this task runner.
    // Execution will occur asynchronously.
    post_task: function(self: PCefTaskRunner; task: PCefTask): Integer; stdcall;

    // Post a task for delayed execution on the thread associated with this task
    // runner. Execution will occur asynchronously. Delayed tasks are not
    // supported on V8 WebWorker threads and will be executed without the
    // specified delay.
    post_delayed_task: function(self: PCefTaskRunner; task: PCefTask;
      delay_ms: Int64): Integer; stdcall;
  end;

  // Structure representing a message. Can be used on any process and thread.
  TCefProcessMessage = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. Do not call any other functions
    // if this function returns false (0).
    is_valid: function(self: PCefProcessMessage): Integer; stdcall;

    // Returns true (1) if the values of this object are read-only. Some APIs may
    // expose read-only objects.
    is_read_only: function(self: PCefProcessMessage): Integer; stdcall;

    // Returns a writable copy of this object.
    copy: function(self: PCefProcessMessage): PCefProcessMessage; stdcall;

    // Returns the message name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_name: function(self: PCefProcessMessage): PCefStringUserFree; stdcall;

    // Returns the list of arguments.
    get_argument_list: function(self: PCefProcessMessage)
      : PCefListValue; stdcall;
  end;

  // Class used to represent a browser window. When used in the browser process
  // the methods of this class may be called on any thread unless otherwise
  // indicated in the comments. When used in the render process the methods of
  // this class may only be called on the main thread.

  TCefBrowser = record
    // Base structure.
    base: TCefBase;

    // Returns the browser host object. This function can only be called in the
    // browser process.
    get_host: function(self: PCefBrowser): PCefBrowserHost; stdcall;

    // Returns true (1) if the browser can navigate backwards.
    can_go_back: function(self: PCefBrowser): Integer; stdcall;

    // Navigate backwards.
    go_back: procedure(self: PCefBrowser); stdcall;

    // Returns true (1) if the browser can navigate forwards.
    can_go_forward: function(self: PCefBrowser): Integer; stdcall;

    // Navigate forwards.
    go_forward: procedure(self: PCefBrowser); stdcall;

    // Returns true (1) if the browser is currently loading.
    is_loading: function(self: PCefBrowser): Integer; stdcall;

    // Reload the current page.
    reload: procedure(self: PCefBrowser); stdcall;

    // Reload the current page ignoring any cached data.
    reload_ignore_cache: procedure(self: PCefBrowser); stdcall;

    // Stop loading the page.
    stop_load: procedure(self: PCefBrowser); stdcall;

    // Returns the globally unique identifier for this frame or < 0 if the
    // underlying frame does not yet exist.
    get_identifier: function(self: PCefBrowser): Integer; stdcall;

    // Returns true (1) if this object is pointing to the same handle as |that|
    // object.
    is_same: function(self, that: PCefBrowser): Integer; stdcall;

    // Returns true (1) if the window is a popup window.
    is_popup: function(self: PCefBrowser): Integer; stdcall;

    // Returns true (1) if a document has been loaded in the browser.
    has_document: function(self: PCefBrowser): Integer; stdcall;

    // Returns the main (top-level) frame for the browser window.
    get_main_frame: function(self: PCefBrowser): PCefFrame; stdcall;

    // Returns the focused frame for the browser window.
    get_focused_frame: function(self: PCefBrowser): PCefFrame; stdcall;

    // Returns the frame with the specified identifier, or NULL if not found.
    get_frame_byident: function(self: PCefBrowser; identifier: Int64)
      : PCefFrame; stdcall;

    // Returns the frame with the specified name, or NULL if not found.
    get_frame: function(self: PCefBrowser; const name: PCefString)
      : PCefFrame; stdcall;

    // Returns the number of frames that currently exist.
    get_frame_count: function(self: PCefBrowser): NativeUInt; stdcall;

    // Returns the identifiers of all existing frames.
    get_frame_identifiers: procedure(self: PCefBrowser;
      identifiersCount: PNativeUInt; identifiers: PInt64); stdcall;

    // Returns the names of all existing frames.
    get_frame_names: procedure(self: PCefBrowser;
      names: TCefStringList); stdcall;

    // Send a message to the specified |target_process|. Returns true (1) if the
    // message was sent successfully.
    send_process_message: function(self: PCefBrowser;
      target_process: TCefProcessId; message: PCefProcessMessage)
      : Integer; stdcall;
  end;

  // Callback structure for cef_browser_host_t::RunFileDialog. The functions of
  // this structure will be called on the browser process UI thread.
  TCefRunFileDialogCallback = record
    // Base structure.
    base: TCefBase;

    // Called asynchronously after the file dialog is dismissed.
    // |selected_accept_filter| is the 0-based index of the value selected from
    // the accept filters array passed to cef_browser_host_t::RunFileDialog.
    // |file_paths| will be a single value or a list of values depending on the
    // dialog mode. If the selection was cancelled |file_paths| will be NULL.
    on_file_dialog_dismissed: procedure(self: PCefRunFileDialogCallback;
      selected_accept_filter: Integer; file_paths: TCefStringList); stdcall;
  end;

  // Callback structure for cef_browser_host_t::GetNavigationEntries. The
  // functions of this structure will be called on the browser process UI thread.
  TCefNavigationEntryVisitor = record
    // Base structure.
    base: TCefBase;

    // Method that will be executed. Do not keep a reference to |entry| outside of
    // this callback. Return true (1) to continue visiting entries or false (0) to
    // stop. |current| is true (1) if this entry is the currently loaded
    // navigation entry. |index| is the 0-based index of this entry and |total| is
    // the total number of entries.
    visit: function(self: PCefNavigationEntryVisitor;
      entry: PCefNavigationEntry; current, index, total: Integer)
      : Integer; stdcall;
  end;

  // Callback structure for cef_browser_host_t::PrintToPDF. The functions of this
  // structure will be called on the browser process UI thread.
  TCefPdfPrintCallback = record
    // Base structure.
    base: TCefBase;

    // Method that will be executed when the PDF printing has completed. |path| is
    // the output path. |ok| will be true (1) if the printing completed
    // successfully or false (0) otherwise.
    on_pdf_print_finished: procedure(self: PCefPdfPrintCallback;
      const path: PCefString; ok: Integer); stdcall;
  end;

  // Structure used to represent the browser process aspects of a browser window.
  // The functions of this structure can only be called in the browser process.
  // They may be called on any thread in that process unless otherwise indicated
  // in the comments.
  TCefBrowserHost = record
    // Base structure.
    base: TCefBase;

    // Returns the hosted browser object.
    get_browser: function(self: PCefBrowserHost): PCefBrowser; stdcall;

    // Request that the browser close. The JavaScript 'onbeforeunload' event will
    // be fired. If |force_close| is false (0) the event handler, if any, will be
    // allowed to prompt the user and the user can optionally cancel the close. If
    // |force_close| is true (1) the prompt will not be displayed and the close
    // will proceed. Results in a call to cef_life_span_handler_t::do_close() if
    // the event handler allows the close or if |force_close| is true (1). See
    // cef_life_span_handler_t::do_close() documentation for additional usage
    // information.
    close_browser: procedure(self: PCefBrowserHost;
      force_close: Integer); stdcall;

     // Set whether the browser is focused.
    set_focus: procedure(self: PCefBrowserHost; focus: Integer); stdcall;

    // Set whether the window containing the browser is visible
    // (minimized/unminimized, app hidden/unhidden, etc). Only used on Mac OS X.
    set_window_visibility: procedure(self: PCefBrowserHost; visible: Integer); stdcall;

    // Retrieve the window handle for this browser.
    get_window_handle: function(self: PCefBrowserHost): TCefWindowHandle; stdcall;

    // Retrieve the window handle of the browser that opened this browser. Will
    // return NULL for non-popup windows. This function can be used in combination
    // with custom handling of modal windows.
    get_opener_window_handle: function(self: PCefBrowserHost): TCefWindowHandle; stdcall;

    // Returns the client for this browser.
    get_client: function(self: PCefBrowserHost): PCefClient; stdcall;

    // Returns the request context for this browser.
    get_request_context: function(self: PCefBrowserHost)
      : PCefRequestContext; stdcall;

    // Get the current zoom level. The default zoom level is 0.0. This function
    // can only be called on the UI thread.
    get_zoom_level: function(self: PCefBrowserHost): Double; stdcall;

    // Change the zoom level to the specified value. Specify 0.0 to reset the zoom
    // level. If called on the UI thread the change will be applied immediately.
    // Otherwise, the change will be applied asynchronously on the UI thread.
    set_zoom_level: procedure(self: PCefBrowserHost;
      zoomLevel: Double); stdcall;

    // Call to run a file chooser dialog. Only a single file chooser dialog may be
    // pending at any given time. |mode| represents the type of dialog to display.
    // |title| to the title to be used for the dialog and may be NULL to show the
    // default title ("Open" or "Save" depending on the mode). |default_file_path|
    // is the path with optional directory and/or file name component that will be
    // initially selected in the dialog. |accept_filters| are used to restrict the
    // selectable file types and may any combination of (a) valid lower-cased MIME
    // types (e.g. "text/*" or "image/*"), (b) individual file extensions (e.g.
    // ".txt" or ".png"), or (c) combined description and file extension delimited
    // using "|" and ";" (e.g. "Image Types|.png;.gif;.jpg").
    // |selected_accept_filter| is the 0-based index of the filter that will be
    // selected by default. |callback| will be executed after the dialog is
    // dismissed or immediately if another dialog is already pending. The dialog
    // will be initiated asynchronously on the UI thread.
    run_file_dialog: procedure(self: PCefBrowserHost; mode: TCefFileDialogMode;
      const title, default_file_path: PCefString; accept_filters: TCefStringList;
      selected_accept_filter: Integer; callback: PCefRunFileDialogCallback); stdcall;

    // Download the file at |url| using cef_download_handler_t.
    start_download: procedure(self: PCefBrowserHost;
      const url: PCefString); stdcall;

    // Print the current browser contents.
    print: procedure(self: PCefBrowserHost); stdcall;

    // Print the current browser contents to the PDF file specified by |path| and
    // execute |callback| on completion. The caller is responsible for deleting
    // |path| when done. For PDF printing to work on Linux you must implement the
    // cef_print_handler_t::GetPdfPaperSize function.
    print_to_pdf: procedure(self: PCefBrowserHost; const path: PCefString;
        const settings: PCefPdfPrintSettings; callback: PCefPdfPrintCallback); stdcall;

    // Search for |searchText|. |identifier| can be used to have multiple searches
    // running simultaniously. |forward| indicates whether to search forward or
    // backward within the page. |matchCase| indicates whether the search should
    // be case-sensitive. |findNext| indicates whether this is the first request
    // or a follow-up. The cef_find_handler_t instance, if any, returned via
    // cef_client_t::GetFindHandler will be called to report find results.
    find: procedure(self: PCefBrowserHost; identifier: Integer;
      const searchText: PCefString;
      forward, matchCase, findNext: Integer); stdcall;

    // Cancel all searches that are currently going on.
    stop_finding: procedure(self: PCefBrowserHost;
      clearSelection: Integer); stdcall;

    // Open developer tools in its own window. If |inspect_element_at| is non-
    // NULL the element at the specified (x,y) location will be inspected.
    show_dev_tools: procedure(self: PCefBrowserHost; const windowInfo: PCefWindowInfo;
        client: PCefClient; const settings: PCefBrowserSettings;
        const inspect_element_at: PCefPoint); stdcall;

    // Explicitly close the developer tools window if one exists for this browser
    // instance.
    close_dev_tools: procedure(self: PCefBrowserHost); stdcall;

    // Retrieve a snapshot of current navigation entries as values sent to the
    // specified visitor. If |current_only| is true (1) only the current
    // navigation entry will be sent, otherwise all navigation entries will be
    // sent.
    get_navigation_entries: procedure(self: PCefBrowserHost;
      visitor: PCefNavigationEntryVisitor; current_only: Integer); stdcall;

    // Set whether mouse cursor change is disabled.
    set_mouse_cursor_change_disabled: procedure(self: PCefBrowserHost;
      disabled: Integer); stdcall;

    // Returns true (1) if mouse cursor change is disabled.
    is_mouse_cursor_change_disabled: function(self: PCefBrowserHost)
      : Integer; stdcall;

    // If a misspelled word is currently selected in an editable node calling this
    // function will replace it with the specified |word|.
    replace_misspelling: procedure(self: PCefBrowserHost;
      const word: PCefString); stdcall;

    // Add the specified |word| to the spelling dictionary.
    add_word_to_dictionary: procedure(self: PCefBrowserHost;
      const word: PCefString); stdcall;

    // Returns true (1) if window rendering is disabled.
    is_window_rendering_disabled: function(self: PCefBrowserHost)
      : Integer; stdcall;

    // Notify the browser that the widget has been resized. The browser will first
    // call cef_render_handler_t::GetViewRect to get the new size and then call
    // cef_render_handler_t::OnPaint asynchronously with the updated regions. This
    // function is only used when window rendering is disabled.
    was_resized: procedure(self: PCefBrowserHost); stdcall;

    // Notify the browser that it has been hidden or shown. Layouting and
    // cef_render_handler_t::OnPaint notification will stop when the browser is
    // hidden. This function is only used when window rendering is disabled.
    was_hidden: procedure(self: PCefBrowserHost; hidden: Integer); stdcall;

    // Send a notification to the browser that the screen info has changed. The
    // browser will then call cef_render_handler_t::GetScreenInfo to update the
    // screen information with the new values. This simulates moving the webview
    // window from one display to another, or changing the properties of the
    // current display. This function is only used when window rendering is
    // disabled.
    notify_screen_info_changed: procedure(self: PCefBrowserHost); stdcall;

    // Invalidate the view. The browser will call cef_render_handler_t::OnPaint
    // asynchronously. This function is only used when window rendering is
    // disabled.
    invalidate: procedure(self: PCefBrowserHost;
      kind: TCefPaintElementType); stdcall;

    // Send a key event to the browser.
    send_key_event: procedure(self: PCefBrowserHost;
      const event: PCefKeyEvent); stdcall;

    // Send a mouse click event to the browser. The |x| and |y| coordinates are
    // relative to the upper-left corner of the view.
    send_mouse_click_event: procedure(self: PCefBrowserHost;
      const event: PCefMouseEvent; kind: TCefMouseButtonType;
      mouseUp, clickCount: Integer); stdcall;

    // Send a mouse move event to the browser. The |x| and |y| coordinates are
    // relative to the upper-left corner of the view.
    send_mouse_move_event: procedure(self: PCefBrowserHost;
      const event: PCefMouseEvent; mouseLeave: Integer); stdcall;

    // Send a mouse wheel event to the browser. The |x| and |y| coordinates are
    // relative to the upper-left corner of the view. The |deltaX| and |deltaY|
    // values represent the movement delta in the X and Y directions respectively.
    // In order to scroll inside select popups with window rendering disabled
    // cef_render_handler_t::GetScreenPoint should be implemented properly.
    send_mouse_wheel_event: procedure(self: PCefBrowserHost;
      const event: PCefMouseEvent; deltaX, deltaY: Integer); stdcall;

    // Send a focus event to the browser.
    send_focus_event: procedure(self: PCefBrowserHost;
      setFocus: Integer); stdcall;

    // Send a capture lost event to the browser.
    send_capture_lost_event: procedure(self: PCefBrowserHost); stdcall;

    // Notify the browser that the window hosting it is about to be moved or
    // resized. This function is only used on Windows and Linux.
    notify_move_or_resize_started: procedure(self: PCefBrowserHost); stdcall;

    // Returns the maximum rate in frames per second (fps) that
    // cef_render_handler_t:: OnPaint will be called for a windowless browser. The
    // actual fps may be lower if the browser cannot generate frames at the
    // requested rate. The minimum value is 1 and the maximum value is 60 (default
    // 30). This function can only be called on the UI thread.
    get_windowless_frame_rate: function(self: PCefBrowserHost): Integer; stdcall;

    // Set the maximum rate in frames per second (fps) that cef_render_handler_t::
    // OnPaint will be called for a windowless browser. The actual fps may be
    // lower if the browser cannot generate frames at the requested rate. The
    // minimum value is 1 and the maximum value is 60 (default 30). Can also be
    // set at browser creation via cef_browser_tSettings.windowless_frame_rate.
    set_windowless_frame_rate: procedure(self: PCefBrowserHost; frame_rate: Integer); stdcall;

    // Get the NSTextInputContext implementation for enabling IME on Mac when
    // window rendering is disabled.
    get_nstext_input_context: function(self: PCefBrowserHost)
      : TCefTextInputContext; stdcall;

    // Handles a keyDown event prior to passing it through the NSTextInputClient
    // machinery.
    handle_key_event_before_text_input_client: procedure(self: PCefBrowserHost;
      keyEvent: TCefEventHandle); stdcall;

    // Performs any additional actions after NSTextInputClient handles the event.
    handle_key_event_after_text_input_client: procedure(self: PCefBrowserHost;
      keyEvent: TCefEventHandle); stdcall;

    // Call this function when the user drags the mouse into the web view (before
    // calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
    // should not contain file contents as this type of data is not allowed to be
    // dragged into the web view. File contents can be removed using
    // cef_drag_data_t::ResetFileContents (for example, if |drag_data| comes from
    // cef_render_handler_t::StartDragging). This function is only used when
    // window rendering is disabled.
    drag_target_drag_enter: procedure(self: PCefBrowserHost;
      drag_data: PCefDragData; const event: PCefMouseEvent;
      allowed_ops: TCefDragOperations); stdcall;

    // Call this function each time the mouse is moved across the web view during
    // a drag operation (after calling DragTargetDragEnter and before calling
    // DragTargetDragLeave/DragTargetDrop). This function is only used when window
    // rendering is disabled.
    drag_target_drag_over: procedure(self: PCefBrowserHost;
      const event: PCefMouseEvent; allowed_ops: TCefDragOperations); stdcall;

    // Call this function when the user drags the mouse out of the web view (after
    // calling DragTargetDragEnter). This function is only used when window
    // rendering is disabled.
    drag_target_drag_leave: procedure(self: PCefBrowserHost); stdcall;

    // Call this function when the user completes the drag operation by dropping
    // the object onto the web view (after calling DragTargetDragEnter). The
    // object being dropped is |drag_data|, given as an argument to the previous
    // DragTargetDragEnter call. This function is only used when window rendering
    // is disabled.
    drag_target_drop: procedure(self: PCefBrowserHost;
      event: PCefMouseEvent); stdcall;

    // Call this function when the drag operation started by a
    // cef_render_handler_t::StartDragging call has ended either in a drop or by
    // being cancelled. |x| and |y| are mouse coordinates relative to the upper-
    // left corner of the view. If the web view is both the drag source and the
    // drag target then all DragTarget* functions should be called before
    // DragSource* mthods. This function is only used when window rendering is
    // disabled.
    drag_source_ended_at: procedure(self: PCefBrowserHost; x, y: Integer;
      op: TCefDragOperation); stdcall;

    // Call this function when the drag operation started by a
    // cef_render_handler_t::StartDragging call has completed. This function may
    // be called immediately without first calling DragSourceEndedAt to cancel a
    // drag operation. If the web view is both the drag source and the drag target
    // then all DragTarget* functions should be called before DragSource* mthods.
    // This function is only used when window rendering is disabled.
    drag_source_system_drag_ended: procedure(self: PCefBrowserHost); stdcall;
  end;

  // Implement this structure to receive string values asynchronously.
  TCefStringVisitor = record
    // Base structure.
    base: TCefBase;

    // Method that will be executed.
    visit: procedure(self: PCefStringVisitor; const str: PCefString); stdcall;
  end;

  // Structure used to represent a frame in the browser window. When used in the
  // browser process the functions of this structure may be called on any thread
  // unless otherwise indicated in the comments. When used in the render process
  // the functions of this structure may only be called on the main thread.
  TCefFrame = record
    // Base structure.
    base: TCefBase;

    // True if this object is currently attached to a valid frame.
    is_valid: function(self: PCefFrame): Integer; stdcall;

    // Execute undo in this frame.
    undo: procedure(self: PCefFrame); stdcall;

    // Execute redo in this frame.
    redo: procedure(self: PCefFrame); stdcall;

    // Execute cut in this frame.
    cut: procedure(self: PCefFrame); stdcall;

    // Execute copy in this frame.
    copy: procedure(self: PCefFrame); stdcall;

    // Execute paste in this frame.
    paste: procedure(self: PCefFrame); stdcall;

    // Execute delete in this frame.
    del: procedure(self: PCefFrame); stdcall;

    // Execute select all in this frame.
    select_all: procedure(self: PCefFrame); stdcall;

    // Save this frame's HTML source to a temporary file and open it in the
    // default text viewing application. This function can only be called from the
    // browser process.
    view_source: procedure(self: PCefFrame); stdcall;

    // Retrieve this frame's HTML source as a string sent to the specified
    // visitor.
    get_source: procedure(self: PCefFrame; visitor: PCefStringVisitor); stdcall;

    // Retrieve this frame's display text as a string sent to the specified
    // visitor.
    get_text: procedure(self: PCefFrame; visitor: PCefStringVisitor); stdcall;

    // Load the request represented by the |request| object.
    load_request: procedure(self: PCefFrame; request: PCefRequest); stdcall;

    // Load the specified |url|.
    load_url: procedure(self: PCefFrame; const url: PCefString); stdcall;

    // Load the contents of |string_val| with the specified dummy |url|. |url|
    // should have a standard scheme (for example, http scheme) or behaviors like
    // link clicks and web security restrictions may not behave as expected.
    load_string: procedure(self: PCefFrame;
      const stringVal, url: PCefString); stdcall;

    // Execute a string of JavaScript code in this frame. The |script_url|
    // parameter is the URL where the script in question can be found, if any. The
    // renderer may request this URL to show the developer the source of the
    // error.  The |start_line| parameter is the base line number to use for error
    // reporting.
    execute_java_script: procedure(self: PCefFrame;
      const code, script_url: PCefString; start_line: Integer); stdcall;

    // Returns true (1) if this is the main (top-level) frame.
    is_main: function(self: PCefFrame): Integer; stdcall;

    // Returns true (1) if this is the focused frame.
    is_focused: function(self: PCefFrame): Integer; stdcall;

    // Returns the name for this frame. If the frame has an assigned name (for
    // example, set via the iframe "name" attribute) then that value will be
    // returned. Otherwise a unique name will be constructed based on the frame
    // parent hierarchy. The main (top-level) frame will always have an NULL name
    // value.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_name: function(self: PCefFrame): PCefStringUserFree; stdcall;

    // Returns the globally unique identifier for this frame.
    get_identifier: function(self: PCefFrame): Int64; stdcall;

    // Returns the parent of this frame or NULL if this is the main (top-level)
    // frame.
    get_parent: function(self: PCefFrame): PCefFrame; stdcall;

    // Returns the URL currently loaded in this frame.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_url: function(self: PCefFrame): PCefStringUserFree; stdcall;

    // Returns the browser that this frame belongs to.
    get_browser: function(self: PCefFrame): PCefBrowser; stdcall;

    // Get the V8 context associated with the frame. This function can only be
    // called from the render process.
    get_v8context: function(self: PCefFrame): PCefv8Context; stdcall;

    // Visit the DOM document. This function can only be called from the render
    // process.
    visit_dom: procedure(self: PCefFrame; visitor: PCefDomVisitor); stdcall;
  end;

  // Structure used to implement a custom resource bundle structure. The functions
  // of this structure may be called on multiple threads.
  TCefResourceBundleHandler = record
    // Base structure.
    base: TCefBase;

    // Called to retrieve a localized translation for the specified |string_id|.
    // To provide the translation set |string| to the translation string and
    // return true (1). To use the default translation return false (0). Include
    // cef_pack_strings.h for a listing of valid string ID values.
    get_localized_string: function(self: PCefResourceBundleHandler;
      string_id: Integer; string_val: PCefString): Integer; stdcall;

    // Called to retrieve data for the specified scale independent |resource_id|.
    // To provide the resource data set |data| and |data_size| to the data pointer
    // and size respectively and return true (1). To use the default resource data
    // return false (0). The resource data will not be copied and must remain
    // resident in memory. Include cef_pack_resources.h for a listing of valid
    // resource ID values.
    get_data_resource: function(self: PCefResourceBundleHandler;
        resource_id: Integer; var data: Pointer; var data_size: NativeUInt): Integer; stdcall;

    // Called to retrieve data for the specified |resource_id| nearest the scale
    // factor |scale_factor|. To provide the resource data set |data| and
    // |data_size| to the data pointer and size respectively and return true (1).
    // To use the default resource data return false (0). The resource data will
    // not be copied and must remain resident in memory. Include
    // cef_pack_resources.h for a listing of valid resource ID values.
    get_data_resource_for_scale: function(self: PCefResourceBundleHandler;
      resource_id: Integer; scale_factor: TCefScaleFactor; out data: Pointer;
      data_size: NativeUInt): Integer; stdcall;
  end;

  // Structure used to create and/or parse command line arguments. Arguments with
  // '--', '-' and, on Windows, '/' prefixes are considered switches. Switches
  // will always precede any arguments without switch prefixes. Switches can
  // optionally have a value specified using the '=' delimiter (e.g.
  // "-switch=value"). An argument of "--" will terminate switch parsing with all
  // subsequent tokens, regardless of prefix, being interpreted as non-switch
  // arguments. Switch names are considered case-insensitive. This structure can
  // be used before cef_initialize() is called.

  TCefCommandLine = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. Do not call any other functions
    // if this function returns false (0).
    is_valid: function(self: PCefCommandLine): Integer; stdcall;

    // Returns true (1) if the values of this object are read-only. Some APIs may
    // expose read-only objects.
    is_read_only: function(self: PCefCommandLine): Integer; stdcall;

    // Returns a writable copy of this object.
    copy: function(self: PCefCommandLine): PCefCommandLine; stdcall;

    // Initialize the command line with the specified |argc| and |argv| values.
    // The first argument must be the name of the program. This function is only
    // supported on non-Windows platforms.
    init_from_argv: procedure(self: PCefCommandLine; argc: Integer;
      const argv: PPAnsiChar); stdcall;

    // Initialize the command line with the string returned by calling
    // GetCommandLineW(). This function is only supported on Windows.
    init_from_string: procedure(self: PCefCommandLine;
      command_line: PCefString); stdcall;

    // Reset the command-line switches and arguments but leave the program
    // component unchanged.
    reset: procedure(self: PCefCommandLine); stdcall;

    // Retrieve the original command line string as a vector of strings. The argv
    // array: { program, [(--|-|/)switch[=value]]*, [--], [argument]* }
    get_argv: procedure(self: PCefCommandLine; argv: TCefStringList); stdcall;

    // Constructs and returns the represented command line string. Use this
    // function cautiously because quoting behavior is unclear.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_command_line_string: function(self: PCefCommandLine)
      : PCefStringUserFree; stdcall;

    // Get the program part of the command line string (the first item).
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_program: function(self: PCefCommandLine): PCefStringUserFree; stdcall;

    // Set the program part of the command line string (the first item).
    set_program: procedure(self: PCefCommandLine;
      program_: PCefString); stdcall;

    // Returns true (1) if the command line has switches.
    has_switches: function(self: PCefCommandLine): Integer; stdcall;

    // Returns true (1) if the command line contains the given switch.
    has_switch: function(self: PCefCommandLine; const name: PCefString)
      : Integer; stdcall;

    // Returns the value associated with the given switch. If the switch has no
    // value or isn't present this function returns the NULL string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_switch_value: function(self: PCefCommandLine; const name: PCefString)
      : PCefStringUserFree; stdcall;

    // Returns the map of switch names and values. If a switch has no value an
    // NULL string is returned.
    get_switches: procedure(self: PCefCommandLine;
      switches: TCefStringMap); stdcall;

    // Add a switch to the end of the command line. If the switch has no value
    // pass an NULL value string.
    append_switch: procedure(self: PCefCommandLine;
      const name: PCefString); stdcall;

    // Add a switch with the specified value to the end of the command line.
    append_switch_with_value: procedure(self: PCefCommandLine;
      const name, value: PCefString); stdcall;

    // True if there are remaining command line arguments.
    has_arguments: function(self: PCefCommandLine): Integer; stdcall;

    // Get the remaining command line arguments.
    get_arguments: procedure(self: PCefCommandLine;
      arguments: TCefStringList); stdcall;

    // Add an argument to the end of the command line.
    append_argument: procedure(self: PCefCommandLine;
      const argument: PCefString); stdcall;

    // Insert a command before the current command. Common for debuggers, like
    // "valgrind" or "gdb --args".
    prepend_wrapper: procedure(self: PCefCommandLine;
      const wrapper: PCefString); stdcall;
  end;

  // Structure used to implement browser process callbacks. The functions of this
  // structure will be called on the browser process main thread unless otherwise
  // indicated.
  TCefBrowserProcessHandler = record
    // Base structure.
    base: TCefBase;

    // Called on the browser process UI thread immediately after the CEF context
    // has been initialized.
    on_context_initialized: procedure(self: PCefBrowserProcessHandler); stdcall;

    // Called before a child process is launched. Will be called on the browser
    // process UI thread when launching a render process and on the browser
    // process IO thread when launching a GPU or plugin process. Provides an
    // opportunity to modify the child process command line. Do not keep a
    // reference to |command_line| outside of this function.
    on_before_child_process_launch: procedure(self: PCefBrowserProcessHandler;
      command_line: PCefCommandLine); stdcall;

    // Called on the browser process IO thread after the main thread has been
    // created for a new render process. Provides an opportunity to specify extra
    // information that will be passed to
    // cef_render_process_handler_t::on_render_thread_created() in the render
    // process. Do not keep a reference to |extra_info| outside of this function.
    on_render_process_thread_created: procedure(self: PCefBrowserProcessHandler;
      extra_info: PCefListValue); stdcall;

    // Return the handler for printing on Linux. If a print handler is not
    // provided then printing will not be supported on the Linux platform.
    get_print_handler: function(self: PCefBrowserProcessHandler)
      : PCefPrintHandler; stdcall;
  end;

  // Structure used to implement render process callbacks. The functions of this
  // structure will be called on the render process main thread (TID_RENDERER)
  // unless otherwise indicated.
  TCefRenderProcessHandler = record
    // Base structure.
    base: TCefBase;

    // Called after the render process main thread has been created. |extra_info|
    // is a read-only value originating from
    // cef_browser_process_handler_t::on_render_process_thread_created(). Do not
    // keep a reference to |extra_info| outside of this function.
    on_render_thread_created: procedure(self: PCefRenderProcessHandler;
      extra_info: PCefListValue); stdcall;

    // Called after WebKit has been initialized.
    on_web_kit_initialized: procedure(self: PCefRenderProcessHandler); stdcall;

    // Called after a browser has been created. When browsing cross-origin a new
    // browser will be created before the old browser with the same identifier is
    // destroyed.
    on_browser_created: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser); stdcall;

    // Called before a browser is destroyed.
    on_browser_destroyed: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser); stdcall;

    // Return the handler for browser load status events.
    get_load_handler: function(self: PCefRenderProcessHandler)
      : PCefLoadHandler; stdcall;

    // Called before browser navigation. Return true (1) to cancel the navigation
    // or false (0) to allow the navigation to proceed. The |request| object
    // cannot be modified in this callback.
    on_before_navigation: function(self: PCefRenderProcessHandler;
      browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
      navigation_type: TCefNavigationType; is_redirect: Integer)
      : Integer; stdcall;

    // Called immediately after the V8 context for a frame has been created. To
    // retrieve the JavaScript 'window' object use the
    // cef_v8context_t::get_global() function. V8 handles can only be accessed
    // from the thread on which they are created. A task runner for posting tasks
    // on the associated thread can be retrieved via the
    // cef_v8context_t::get_task_runner() function.
    on_context_created: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context); stdcall;

    // Called immediately before the V8 context for a frame is released. No
    // references to the context should be kept after this function is called.
    on_context_released: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context); stdcall;

    // Called for global uncaught exceptions in a frame. Execution of this
    // callback is disabled by default. To enable set
    // CefSettings.uncaught_exception_stack_size > 0.
    on_uncaught_exception: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context;
      exception: PCefV8Exception; stackTrace: PCefV8StackTrace); stdcall;

    // Called when a new node in the the browser gets focus. The |node| value may
    // be NULL if no specific node has gained focus. The node object passed to
    // this function represents a snapshot of the DOM at the time this function is
    // executed. DOM objects are only valid for the scope of this function. Do not
    // keep references to or attempt to access any DOM objects outside the scope
    // of this function.
    on_focused_node_changed: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; frame: PCefFrame; node: PCefDomNode); stdcall;

    // Called when a new message is received from a different process. Return true
    // (1) if the message was handled or false (0) otherwise. Do not keep a
    // reference to or attempt to access the message outside of this callback.
    on_process_message_received: function(self: PCefRenderProcessHandler;
      browser: PCefBrowser; source_process: TCefProcessId;
      message: PCefProcessMessage): Integer; stdcall;
  end;

  // Implement this structure to provide handler implementations. Methods will be
  // called by the process and/or thread indicated.
  TCefApp = record
    // Base structure.
    base: TCefBase;

    // Provides an opportunity to view and/or modify command-line arguments before
    // processing by CEF and Chromium. The |process_type| value will be NULL for
    // the browser process. Do not keep a reference to the cef_command_line_t
    // object passed to this function. The CefSettings.command_line_args_disabled
    // value can be used to start with an NULL command-line object. Any values
    // specified in CefSettings that equate to command-line arguments will be set
    // before this function is called. Be cautious when using this function to
    // modify command-line arguments for non-browser processes as this may result
    // in undefined behavior including crashes.
    on_before_command_line_processing: procedure(self: PCefApp;
      const process_type: PCefString; command_line: PCefCommandLine); stdcall;

    // Provides an opportunity to register custom schemes. Do not keep a reference
    // to the |registrar| object. This function is called on the main thread for
    // each process and the registered schemes should be the same across all
    // processes.
    on_register_custom_schemes: procedure(self: PCefApp;
      registrar: PCefSchemeRegistrar); stdcall;

    // Return the handler for resource bundle events. If
    // CefSettings.pack_loading_disabled is true (1) a handler must be returned.
    // If no handler is returned resources will be loaded from pack files. This
    // function is called by the browser and render processes on multiple threads.
    get_resource_bundle_handler: function(self: PCefApp)
      : PCefResourceBundleHandler; stdcall;

    // Return the handler for functionality specific to the browser process. This
    // function is called on multiple threads in the browser process.
    get_browser_process_handler: function(self: PCefApp)
      : PCefBrowserProcessHandler; stdcall;

    // Return the handler for functionality specific to the render process. This
    // function is called on the render process main thread.
    get_render_process_handler: function(self: PCefApp)
      : PCefRenderProcessHandler; stdcall;
  end;

  // Implement this structure to handle events related to browser life span. The
  // functions of this structure will be called on the UI thread unless otherwise
  // indicated.
  TCefLifeSpanHandler = record
    // Base structure.
    base: TCefBase;

	// Called on the IO thread before a new popup browser is created. The
    // |browser| and |frame| values represent the source of the popup request. The
    // |target_url| and |target_frame_name| values indicate where the popup
    // browser should navigate and may be NULL if not specified with the request.
    // The |target_disposition| value indicates where the user intended to open
    // the popup (e.g. current tab, new tab, etc). The |user_gesture| value will
    // be true (1) if the popup was opened via explicit user gesture (e.g.
    // clicking a link) or false (0) if the popup opened automatically (e.g. via
    // the DomContentLoaded event). The |popupFeatures| structure contains
    // additional information about the requested popup window. To allow creation
    // of the popup browser optionally modify |windowInfo|, |client|, |settings|
    // and |no_javascript_access| and return false (0). To cancel creation of the
    // popup browser return true (1). The |client| and |settings| values will
    // default to the source browser's values. If the |no_javascript_access| value
    // is set to false (0) the new browser will not be scriptable and may not be
    // hosted in the same renderer process as the source browser.
    on_before_popup: function(self: PCefLifeSpanHandler;
      browser: PCefBrowser; frame: PCefFrame;
      const target_url, target_frame_name: PCefString;
      target_disposition: TCefWindowOpenDisposition; user_gesture: Integer;
      const popupFeatures: PCefPopupFeatures;
      windowInfo: PCefWindowInfo; var client: PCefClient;
      settings: PCefBrowserSettings; no_javascript_access: PInteger): Integer; stdcall;

    // Called after a new browser is created.
    on_after_created: procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;

    // Called when a modal window is about to display and the modal loop should
    // begin running. Return false (0) to use the default modal loop
    // implementation or true (1) to use a custom implementation.
    run_modal: function(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;

    // Called when a browser has recieved a request to close. This may result
    // directly from a call to cef_browser_host_t::close_browser() or indirectly
    // if the browser is a top-level OS window created by CEF and the user
    // attempts to close the window. This function will be called after the
    // JavaScript 'onunload' event has been fired. It will not be called for
    // browsers after the associated OS window has been destroyed (for those
    // browsers it is no longer possible to cancel the close).
    //
    // If CEF created an OS window for the browser returning false (0) will send
    // an OS close notification to the browser window's top-level owner (e.g.
    // WM_CLOSE on Windows, performClose: on OS-X and "delete_event" on Linux). If
    // no OS window exists (window rendering disabled) returning false (0) will
    // cause the browser object to be destroyed immediately. Return true (1) if
    // the browser is parented to another window and that other window needs to
    // receive close notification via some non-standard technique.
    //
    // If an application provides its own top-level window it should handle OS
    // close notifications by calling cef_browser_host_t::CloseBrowser(false (0))
    // instead of immediately closing (see the example below). This gives CEF an
    // opportunity to process the 'onbeforeunload' event and optionally cancel the
    // close before do_close() is called.
    //
    // The cef_life_span_handler_t::on_before_close() function will be called
    // immediately before the browser object is destroyed. The application should
    // only exit after on_before_close() has been called for all existing
    // browsers.
    //
    // If the browser represents a modal window and a custom modal loop
    // implementation was provided in cef_life_span_handler_t::run_modal() this
    // callback should be used to restore the opener window to a usable state.
    //
    // By way of example consider what should happen during window close when the
    // browser is parented to an application-provided top-level OS window. 1.
    // User clicks the window close button which sends an OS close
    //     notification (e.g. WM_CLOSE on Windows, performClose: on OS-X and
    //     "delete_event" on Linux).
    // 2.  Application's top-level window receives the close notification and:
    //     A. Calls CefBrowserHost::CloseBrowser(false).
    //     B. Cancels the window close.
    // 3.  JavaScript 'onbeforeunload' handler executes and shows the close
    //     confirmation dialog (which can be overridden via
    //     CefJSDialogHandler::OnBeforeUnloadDialog()).
    // 4.  User approves the close. 5.  JavaScript 'onunload' handler executes. 6.
    // Application's do_close() handler is called. Application will:
    //     A. Set a flag to indicate that the next close attempt will be allowed.
    //     B. Return false.
    // 7.  CEF sends an OS close notification. 8.  Application's top-level window
    // receives the OS close notification and
    //     allows the window to close based on the flag from #6B.
    // 9.  Browser OS window is destroyed. 10. Application's
    // cef_life_span_handler_t::on_before_close() handler is called and
    //     the browser object is destroyed.
    // 11. Application exits by calling cef_quit_message_loop() if no other
    // browsers
    //     exist.
    do_close: function(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;

    // Called just before a browser is destroyed. Release all references to the
    // browser object and do not attempt to execute any functions on the browser
    // object after this callback returns. If this is a modal window and a custom
    // modal loop implementation was provided in run_modal() this callback should
    // be used to exit the custom modal loop. See do_close() documentation for
    // additional usage information.
    on_before_close: procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
  end;



  // Implement this structure to handle events related to browser load status. The

  // functions of this structure will be called on the UI thread.
  TCefLoadHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the loading state has changed. This callback will be executed
    // twice -- once when loading is initiated either programmatically or by user
    // action, and once when loading is terminated due to completion, cancellation
    // of failure. It will be called before any calls to OnLoadStart and after all
    // calls to OnLoadError and/or OnLoadEnd.
    on_loading_state_change: procedure(self: PCefLoadHandler;
      browser: PCefBrowser; isLoading, canGoBack, canGoForward
      : Integer); stdcall;

    // Called when the browser begins loading a frame. The |frame| value will
    // never be NULL -- call the is_main() function to check if this frame is the
    // main frame. Multiple frames may be loading at the same time. Sub-frames may
    // start or continue loading after the main frame load has ended. This
    // function will always be called for all frames irrespective of whether the
    // request completes successfully. For notification of overall browser load
    // status use OnLoadingStateChange instead.
    on_load_start: procedure(self: PCefLoadHandler; browser: PCefBrowser;
      frame: PCefFrame); stdcall;

    // Called when the browser is done loading a frame. The |frame| value will
    // never be NULL -- call the is_main() function to check if this frame is the
    // main frame. Multiple frames may be loading at the same time. Sub-frames may
    // start or continue loading after the main frame load has ended. This
    // function will always be called for all frames irrespective of whether the
    // request completes successfully. For notification of overall browser load
    // status use OnLoadingStateChange instead.
    on_load_end: procedure(self: PCefLoadHandler; browser: PCefBrowser;
      frame: PCefFrame; httpStatusCode: Integer); stdcall;

    // Called when the resource load for a navigation fails or is canceled.
    // |errorCode| is the error code number, |errorText| is the error text and
    // |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
    // for complete descriptions of the error codes.
    on_load_error: procedure(self: PCefLoadHandler; browser: PCefBrowser;
      frame: PCefFrame; errorCode: Integer;
      const errorText, failedUrl: PCefString); stdcall;
  end;

  // Generic callback structure used for asynchronous continuation.
  TCefCallback = record
    // Base structure.
    base: TCefBase;

    // Continue processing.
    cont: procedure(self: PCefCallback); stdcall;

    // Cancel processing.
    cancel: procedure(self: PCefCallback); stdcall;
  end;

  // Generic callback structure used for asynchronous completion.
  TCefCompletionCallback = record
    // Base structure.
    base: TCefBase;
    // Method that will be called once the task is complete.
    on_complete: procedure(self: PCefCompletionCallback); stdcall;
  end;

  // Structure used to implement a custom request handler structure. The functions
  // of this structure will always be called on the IO thread.
  TCefResourceHandler = record
    // Base structure.
    base: TCefBase;

    // Begin processing the request. To handle the request return true (1) and
    // call cef_callback_t::cont() once the response header information is
    // available (cef_callback_t::cont() can also be called from inside this
    // function if header information is available immediately). To cancel the
    // request return false (0).
    process_request: function(self: PCefResourceHandler; request: PCefRequest;
      callback: PCefCallback): Integer; stdcall;

	// Retrieve response header information. If the response length is not known
    // set |response_length| to -1 and read_response() will be called until it
    // returns false (0). If the response length is known set |response_length| to
    // a positive value and read_response() will be called until it returns false
    // (0) or the specified number of bytes have been read. Use the |response|
    // object to set the mime type, http status code and other optional header
    // values. To redirect the request to a new URL set |redirectUrl| to the new
    // URL.
    get_response_headers: procedure(self: PCefResourceHandler;
      response: PCefResponse; response_length: PInt64; redirectUrl: PCefString); stdcall;

    // Read response data. If data is available immediately copy up to
    // |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
    // bytes copied, and return true (1). To read the data at a later time set
    // |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
    // data is available. To indicate response completion return false (0).
    read_response: function(self: PCefResourceHandler; data_out: Pointer;
      bytes_to_read: Integer; bytes_read: PInteger; callback: PCefCallback)
      : Integer; stdcall;

    // Return true (1) if the specified cookie can be sent with the request or
    // false (0) otherwise. If false (0) is returned for any cookie then no
    // cookies will be sent with the request.
    can_get_cookie: function(self: PCefResourceHandler;
      const cookie: PCefCookie): Integer; stdcall;

    // Return true (1) if the specified cookie returned with the response can be
    // set or false (0) otherwise.
    can_set_cookie: function(self: PCefResourceHandler;
      const cookie: PCefCookie): Integer; stdcall;

    // Request processing has been canceled.
    cancel: procedure(self: PCefResourceHandler); stdcall;
  end;

  // Callback structure used for asynchronous continuation of authentication
  // requests.
  TCefAuthCallback = record
    // Base structure.
    base: TCefBase;

    // Continue the authentication request.
    cont: procedure(self: PCefAuthCallback;
      const username, password: PCefString); stdcall;

    // Cancel the authentication request.
    cancel: procedure(self: PCefAuthCallback); stdcall;
  end;

  // Callback structure used for asynchronous continuation of quota requests.
  TCefRequestCallback = record
    // Base structure.
    base: TCefBase;

    // Continue the url request. If |allow| is true (1) the request will be
    // continued. Otherwise, the request will be canceled.
    cont: procedure(self: PCefRequestCallback; allow: Integer); stdcall;
    // Cancel the quota request.
    cancel: procedure(self: PCefRequestCallback); stdcall;
  end;

  // Implement this structure to handle events related to browser requests. The
  // functions of this structure will be called on the thread indicated.
  TCefRequestHandler = record
    // Base structure.
    base: TCefBase;

    // Called on the UI thread before browser navigation. Return true (1) to
    // cancel the navigation or false (0) to allow the navigation to proceed. The
    // |request| object cannot be modified in this callback.
    // cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
    // If the navigation is allowed cef_load_handler_t::OnLoadStart and
    // cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
    // cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
    // ERR_ABORTED.
    on_before_browse: function(self: PCefRequestHandler; browser: PCefBrowser;
     frame: PCefFrame; request: PCefRequest; isRedirect: Integer): Integer; stdcall;

    // Called on the UI thread before OnBeforeBrowse in certain limited cases
    // where navigating a new or different browser might be desirable. This
    // includes user-initiated navigation that might open in a special way (e.g.
    // links clicked via middle-click or ctrl + left-click) and certain types of
    // cross-origin navigation initiated from the renderer process (e.g.
    // navigating the top-level frame to/from a file URL). The |browser| and
    // |frame| values represent the source of the navigation. The
    // |target_disposition| value indicates where the user intended to navigate
    // the browser based on standard Chromium behaviors (e.g. current tab, new
    // tab, etc). The |user_gesture| value will be true (1) if the browser
    // navigated via explicit user gesture (e.g. clicking a link) or false (0) if
    // it navigated automatically (e.g. via the DomContentLoaded event). Return
    // true (1) to cancel the navigation or false (0) to allow the navigation to
    // proceed in the source browser's top-level frame.
    on_open_urlfrom_tab: function(self: PCefRequestHandler; browser:PCefBrowser;
      frame: PCefFrame; const target_url: PCefString;
      target_disposition: TCefWindowOpenDisposition; user_gesture: Integer): Integer; stdcall;

    // Called on the IO thread before a resource request is loaded. The |request|
    // object may be modified. To cancel the request return true (1) otherwise
    // return false (0).
    on_before_resource_load: function(self: PCefRequestHandler;
      browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
      callback: PCefRequestCallback): TCefReturnValue; stdcall;

    // Called on the IO thread before a resource is loaded. To allow the resource
    // to load normally return NULL. To specify a handler for the resource return
    // a cef_resource_handler_t object. The |request| object should not be
    // modified in this callback.
    get_resource_handler: function(self: PCefRequestHandler;
      browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; stdcall;

    // Called on the IO thread when a resource load is redirected. The |request|
    // parameter will contain the old URL and other request-related information.
    // The |new_url| parameter will contain the new URL and can be changed if
    // desired. The |request| object cannot be modified in this callback.
    on_resource_redirect: procedure(self: PCefRequestHandler;
      browser: PCefBrowser; frame: PCefFrame; const request: PCefRequest;
      new_url: PCefString); stdcall;

    // Called on the IO thread when a resource response is received. To allow the
    // resource to load normally return false (0). To redirect or retry the
    // resource modify |request| (url, headers or post body) and return true (1).
    // The |response| object cannot be modified in this callback.
    on_resource_response: function(self: PCefRequestHandler;
        browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
        response: PCefResponse): Integer; stdcall;

    // Called on the IO thread to optionally filter resource response content.
    // |request| and |response| represent the request and response respectively
    // and cannot be modified in this callback.
    get_resource_response_filter: function(self: PCefRequestHandler; browser: PCefBrowser;
      frame: PCefFrame; request: PCefRequest; response: PCefResponse): PCefResponseFilter; stdcall;

    // Called on the IO thread when a resource load has completed. |request| and
    // |response| represent the request and response respectively and cannot be
    // modified in this callback. |status| indicates the load completion status.
    // |received_content_length| is the number of response bytes actually read.
    on_resource_load_complete: procedure(self: PCefRequestHandler; browser: PCefBrowser;
      frame: PCefFrame; request: PCefRequest; response: PCefResponse;
      status: TCefUrlRequestStatus; received_content_length: Int64); stdcall;

    // Called on the IO thread when the browser needs credentials from the user.
    // |isProxy| indicates whether the host is a proxy server. |host| contains the
    // hostname and |port| contains the port number. |realm| is the realm of the
    // challenge and may be NULL. |scheme| is the authentication scheme used, such
    // as "basic" or "digest", and will be NULL if the source of the request is an
    // FTP server. Return true (1) to continue the request and call
    // cef_auth_callback_t::cont() either in this function or at a later time when
    // the authentication information is available. Return false (0) to cancel the
    // request immediately.
    get_auth_credentials: function(self: PCefRequestHandler;
      browser: PCefBrowser; frame: PCefFrame; isProxy: Integer; const host: PCefString;
      port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;

    // Called on the IO thread when JavaScript requests a specific storage quota
    // size via the webkitStorageInfo.requestQuota function. |origin_url| is the
    // origin of the page making the request. |new_size| is the requested quota
    // size in bytes. Return true (1) to continue the request and call
    // cef_request_tCallback::cont() either in this function or at a later time to
    // grant or deny the request. Return false (0) to cancel the request
    // immediately.
    on_quota_request: function(self: PCefRequestHandler; browser: PCefBrowser;
      const origin_url: PCefString; new_size: Int64; callback: PCefRequestCallback): Integer; stdcall;

    // Called on the UI thread to handle requests for URLs with an unknown
    // protocol component. Set |allow_os_execution| to true (1) to attempt
    // execution via the registered OS protocol handler, if any. SECURITY WARNING:
    // YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
    // OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
    on_protocol_execution: procedure(self: PCefRequestHandler;
      browser: PCefBrowser; const url: PCefString; allow_os_execution: PInteger); stdcall;

    // Called on the UI thread to handle requests for URLs with an invalid SSL
    // certificate. Return true (1) and call cef_request_tCallback::cont() either
    // in this function or at a later time to continue or cancel the request.
    // Return false (0) to cancel the request immediately. If
    // CefSettings.ignore_certificate_errors is set all invalid certificates will
    // be accepted without calling this function.
    on_certificate_error: function(self: PCefRequestHandler;
      browser: PCefBrowser; cert_error: TCefErrorcode;
      const request_url: PCefString; ssl_info: PCefSslInfo;
      callback: PCefRequestCallback): Integer; stdcall;

    // Called on the browser process UI thread when a plugin has crashed.
    // |plugin_path| is the path of the plugin that crashed.
    on_plugin_crashed: procedure(self: PCefRequestHandler; browser: PCefBrowser;
      const plugin_path: PCefString); stdcall;

    // Called on the browser process UI thread when the render view associated
    // with |browser| is ready to receive/handle IPC messages in the render
    // process.
    on_render_view_ready: procedure(self: PCefRequestHandler; browser: PCefBrowser); stdcall;

    // Called on the browser process UI thread when the render process terminates
    // unexpectedly. |status| indicates how the process terminated.
    on_render_process_terminated: procedure(self: PCefRequestHandler; browser: PCefBrowser;
      status: TCefTerminationStatus); stdcall;
  end;

  // Implement this structure to handle events related to browser display state.

  // The functions of this structure will be called on the UI thread.
  TCefDisplayHandler = record
    // Base structure.
    base: TCefBase;

    // Called when a frame's address has changed.
    on_address_change: procedure(self: PCefDisplayHandler; browser: PCefBrowser;
      frame: PCefFrame; const url: PCefString); stdcall;

    // Called when the page title changes.
    on_title_change: procedure(self: PCefDisplayHandler; browser: PCefBrowser;
      const title: PCefString); stdcall;

    // Called when the page icon changes.
    on_favicon_urlchange: procedure(self: PCefDisplayHandler;
        browser: PCefBrowser; icon_urls: TCefStringList); stdcall;

    // Called when web content in the page has toggled fullscreen mode. If
    // |fullscreen| is true (1) the content will automatically be sized to fill
    // the browser content area. If |fullscreen| is false (0) the content will
    // automatically return to its original size and position. The client is
    // responsible for resizing the browser if desired.
    on_fullscreen_mode_change: procedure(self: PCefDisplayHandler;
        browser: PCefBrowser; fullscreen: Integer); stdcall;

    // Called when the browser is about to display a tooltip. |text| contains the
    // text that will be displayed in the tooltip. To handle the display of the
    // tooltip yourself return true (1). Otherwise, you can optionally modify
    // |text| and then return false (0) to allow the browser to display the
    // tooltip. When window rendering is disabled the application is responsible
    // for drawing tooltips and the return value is ignored.
    on_tooltip: function(self: PCefDisplayHandler; browser: PCefBrowser;
      text: PCefString): Integer; stdcall;

    // Called when the browser receives a status message. |value| contains the
    // text that will be displayed in the status message.
    on_status_message: procedure(self: PCefDisplayHandler; browser: PCefBrowser;
      const value: PCefString); stdcall;

    // Called to display a console message. Return true (1) to stop the message
    // from being output to the console.
    on_console_message: function(self: PCefDisplayHandler; browser: PCefBrowser;
      const message: PCefString; const source: PCefString; line: Integer)
      : Integer; stdcall;
  end;

  // Implement this structure to handle events related to focus. The functions of
  // this structure will be called on the UI thread.
  TCefFocusHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the browser component is about to loose focus. For instance, if
    // focus was on the last HTML element and the user pressed the TAB key. |next|
    // will be true (1) if the browser is giving focus to the next component and
    // false (0) if the browser is giving focus to the previous component.
    on_take_focus: procedure(self: PCefFocusHandler; browser: PCefBrowser;
      next: Integer); stdcall;

    // Called when the browser component is requesting focus. |source| indicates
    // where the focus request is originating from. Return false (0) to allow the
    // focus to be set or true (1) to cancel setting the focus.
    on_set_focus: function(self: PCefFocusHandler; browser: PCefBrowser;
      source: TCefFocusSource): Integer; stdcall;

    // Called when the browser component has received focus.
    on_got_focus: procedure(self: PCefFocusHandler;
      browser: PCefBrowser); stdcall;
  end;

  // Implement this structure to handle events related to keyboard input. The
  // functions of this structure will be called on the UI thread.
  TCefKeyboardHandler = record
    // Base structure.
    base: TCefBase;

    // Called before a keyboard event is sent to the renderer. |event| contains
    // information about the keyboard event. |os_event| is the operating system
    // event message, if any. Return true (1) if the event was handled or false
    // (0) otherwise. If the event will be handled in on_key_event() as a keyboard
    // shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
    on_pre_key_event: function(self: PCefKeyboardHandler; browser: PCefBrowser;
      const event: PCefKeyEvent; os_event: TCefEventHandle;
      is_keyboard_shortcut: PInteger): Integer; stdcall;

    // Called after the renderer and JavaScript in the page has had a chance to
    // handle the event. |event| contains information about the keyboard event.
    // |os_event| is the operating system event message, if any. Return true (1)
    // if the keyboard event was handled or false (0) otherwise.
    on_key_event: function(self: PCefKeyboardHandler; browser: PCefBrowser;
      const event: PCefKeyEvent; os_event: TCefEventHandle): Integer; stdcall;
  end;

  // Callback structure used for asynchronous continuation of JavaScript dialog
  // requests.
  TCefJsDialogCallback = record
    // Base structure.
    base: TCefBase;

    // Continue the JS dialog request. Set |success| to true (1) if the OK button
    // was pressed. The |user_input| value should be specified for prompt dialogs.
    cont: procedure(self: PCefJsDialogCallback; success: Integer;
      const user_input: PCefString); stdcall;
  end;

  // Implement this structure to handle events related to JavaScript dialogs. The
  // functions of this structure will be called on the UI thread.
  TCefJsDialogHandler = record
    // Base structure.
    base: TCefBase;

	// Called to run a JavaScript dialog. If |origin_url| and |accept_lang| are
    // non-NULL they can be passed to the CefFormatUrlForSecurityDisplay function
    // to retrieve a secure and user-friendly display string. The
    // |default_prompt_text| value will be specified for prompt dialogs only. Set
    // |suppress_message| to true (1) and return false (0) to suppress the message
    // (suppressing messages is preferable to immediately executing the callback
    // as this is used to detect presumably malicious behavior like spamming alert
    // messages in onbeforeunload). Set |suppress_message| to false (0) and return
    // false (0) to use the default implementation (the default implementation
    // will show one modal dialog at a time and suppress any additional dialog
    // requests until the displayed dialog is dismissed). Return true (1) if the
    // application will use a custom dialog or if the callback has been executed
    // immediately. Custom dialogs may be either modal or modeless. If a custom
    // dialog is used the application must execute |callback| once the custom
    // dialog is dismissed.
    on_jsdialog: function(self: PCefJsDialogHandler;
      browser: PCefBrowser; const origin_url, accept_lang: PCefString;
      dialog_type: TCefJsDialogType; const message_text, default_prompt_text: PCefString;
      callback: PCefJsDialogCallback; suppress_message: PInteger): Integer; stdcall;

    // Called to run a dialog asking the user if they want to leave a page. Return
    // false (0) to use the default dialog implementation. Return true (1) if the
    // application will use a custom dialog or if the callback has been executed
    // immediately. Custom dialogs may be either modal or modeless. If a custom
    // dialog is used the application must execute |callback| once the custom
    // dialog is dismissed.
    on_before_unload_dialog: function(self: PCefJsDialogHandler;
      browser: PCefBrowser; const message_text: PCefString; is_reload: Integer;
      callback: PCefJsDialogCallback): Integer; stdcall;

    // Called to cancel any pending dialogs and reset any saved dialog state. Will
    // be called due to events like page navigation irregardless of whether any
    // dialogs are currently pending.
    on_reset_dialog_state: procedure(self: PCefJsDialogHandler;
      browser: PCefBrowser); stdcall;

    // Called when the default implementation dialog is closed.
    on_dialog_closed: procedure(self: PCefJsDialogHandler;
      browser: PCefBrowser); stdcall;
  end;

  // Supports creation and modification of menus. See cef_menu_id_t for the
  // command ids that have default implementations. All user-defined command ids
  // should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
  // this structure can only be accessed on the browser process the UI thread.
  TCefMenuModel = record
    // Base structure.
    base: TCefBase;

    // Clears the menu. Returns true (1) on success.
    clear: function(self: PCefMenuModel): Integer; stdcall;

    // Returns the number of items in this menu.
    get_count: function(self: PCefMenuModel): Integer; stdcall;

    // Add a separator to the menu. Returns true (1) on success.
    add_separator: function(self: PCefMenuModel): Integer; stdcall;

    // Add an item to the menu. Returns true (1) on success.
    add_item: function(self: PCefMenuModel; command_id: Integer;
      const text: PCefString): Integer; stdcall;

    // Add a check item to the menu. Returns true (1) on success.
    add_check_item: function(self: PCefMenuModel; command_id: Integer;
      const text: PCefString): Integer; stdcall;

    // Add a radio item to the menu. Only a single item with the specified
    // |group_id| can be checked at a time. Returns true (1) on success.
    add_radio_item: function(self: PCefMenuModel; command_id: Integer;
      const text: PCefString; group_id: Integer): Integer; stdcall;

    // Add a sub-menu to the menu. The new sub-menu is returned.
    add_sub_menu: function(self: PCefMenuModel; command_id: Integer;
      const text: PCefString): PCefMenuModel; stdcall;

    // Insert a separator in the menu at the specified |index|. Returns true (1)
    // on success.
    insert_separator_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Insert an item in the menu at the specified |index|. Returns true (1) on
    // success.
    insert_item_at: function(self: PCefMenuModel; index, command_id: Integer;
      const text: PCefString): Integer; stdcall;

    // Insert a check item in the menu at the specified |index|. Returns true (1)
    // on success.
    insert_check_item_at: function(self: PCefMenuModel;
      index, command_id: Integer; const text: PCefString): Integer; stdcall;

    // Insert a radio item in the menu at the specified |index|. Only a single
    // item with the specified |group_id| can be checked at a time. Returns true
    // (1) on success.
    insert_radio_item_at: function(self: PCefMenuModel;
      index, command_id: Integer; const text: PCefString; group_id: Integer)
      : Integer; stdcall;

    // Insert a sub-menu in the menu at the specified |index|. The new sub-menu is
    // returned.
    insert_sub_menu_at: function(self: PCefMenuModel;
      index, command_id: Integer; const text: PCefString)
      : PCefMenuModel; stdcall;

    // Removes the item with the specified |command_id|. Returns true (1) on
    // success.
    remove: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Removes the item at the specified |index|. Returns true (1) on success.
    remove_at: function(self: PCefMenuModel; index: Integer): Integer; stdcall;

    // Returns the index associated with the specified |command_id| or -1 if not
    // found due to the command id not existing in the menu.
    get_index_of: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Returns the command id at the specified |index| or -1 if not found due to
    // invalid range or the index being a separator.
    get_command_id_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Sets the command id at the specified |index|. Returns true (1) on success.
    set_command_id_at: function(self: PCefMenuModel; index, command_id: Integer)
      : Integer; stdcall;

    // Returns the label for the specified |command_id| or NULL if not found.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_label: function(self: PCefMenuModel; command_id: Integer)
      : PCefStringUserFree; stdcall;

    // Returns the label at the specified |index| or NULL if not found due to
    // invalid range or the index being a separator.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_label_at: function(self: PCefMenuModel; index: Integer)
      : PCefStringUserFree; stdcall;

    // Sets the label for the specified |command_id|. Returns true (1) on success.
    set_label: function(self: PCefMenuModel; command_id: Integer;
      const text: PCefString): Integer; stdcall;

    // Set the label at the specified |index|. Returns true (1) on success.
    set_label_at: function(self: PCefMenuModel; index: Integer;
      const text: PCefString): Integer; stdcall;

    // Returns the item type for the specified |command_id|.
    get_type: function(self: PCefMenuModel; command_id: Integer)
      : TCefMenuItemType; stdcall;

    // Returns the item type at the specified |index|.
    get_type_at: function(self: PCefMenuModel; index: Integer)
      : TCefMenuItemType; stdcall;

    // Returns the group id for the specified |command_id| or -1 if invalid.
    get_group_id: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Returns the group id at the specified |index| or -1 if invalid.
    get_group_id_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Sets the group id for the specified |command_id|. Returns true (1) on
    // success.
    set_group_id: function(self: PCefMenuModel; command_id, group_id: Integer)
      : Integer; stdcall;

    // Sets the group id at the specified |index|. Returns true (1) on success.
    set_group_id_at: function(self: PCefMenuModel; index, group_id: Integer)
      : Integer; stdcall;

    // Returns the submenu for the specified |command_id| or NULL if invalid.
    get_sub_menu: function(self: PCefMenuModel; command_id: Integer)
      : PCefMenuModel; stdcall;

    // Returns the submenu at the specified |index| or NULL if invalid.
    get_sub_menu_at: function(self: PCefMenuModel; index: Integer)
      : PCefMenuModel; stdcall;

    // Returns true (1) if the specified |command_id| is visible.
    is_visible: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |index| is visible.
    is_visible_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Change the visibility of the specified |command_id|. Returns true (1) on
    // success.
    set_visible: function(self: PCefMenuModel; command_id, visible: Integer)
      : Integer; stdcall;

    // Change the visibility at the specified |index|. Returns true (1) on
    // success.
    set_visible_at: function(self: PCefMenuModel; index, visible: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |command_id| is enabled.
    is_enabled: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |index| is enabled.
    is_enabled_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Change the enabled status of the specified |command_id|. Returns true (1)
    // on success.
    set_enabled: function(self: PCefMenuModel; command_id, enabled: Integer)
      : Integer; stdcall;

    // Change the enabled status at the specified |index|. Returns true (1) on
    // success.
    set_enabled_at: function(self: PCefMenuModel; index, enabled: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |command_id| is checked. Only applies to
    // check and radio items.
    is_checked: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |index| is checked. Only applies to check
    // and radio items.
    is_checked_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Check the specified |command_id|. Only applies to check and radio items.
    // Returns true (1) on success.
    set_checked: function(self: PCefMenuModel; command_id, checked: Integer)
      : Integer; stdcall;

    // Check the specified |index|. Only applies to check and radio items. Returns
    // true (1) on success.
    set_checked_at: function(self: PCefMenuModel; index, checked: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |command_id| has a keyboard accelerator
    // assigned.
    has_accelerator: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Returns true (1) if the specified |index| has a keyboard accelerator
    // assigned.
    has_accelerator_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Set the keyboard accelerator for the specified |command_id|. |key_code| can
    // be any virtual key or character value. Returns true (1) on success.
    set_accelerator: function(self: PCefMenuModel;
      command_id, key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer)
      : Integer; stdcall;

    // Set the keyboard accelerator at the specified |index|. |key_code| can be
    // any virtual key or character value. Returns true (1) on success.
    set_accelerator_at: function(self: PCefMenuModel;
      index, key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer)
      : Integer; stdcall;

    // Remove the keyboard accelerator for the specified |command_id|. Returns
    // true (1) on success.
    remove_accelerator: function(self: PCefMenuModel; command_id: Integer)
      : Integer; stdcall;

    // Remove the keyboard accelerator at the specified |index|. Returns true (1)
    // on success.
    remove_accelerator_at: function(self: PCefMenuModel; index: Integer)
      : Integer; stdcall;

    // Retrieves the keyboard accelerator for the specified |command_id|. Returns
    // true (1) on success.
    get_accelerator: function(self: PCefMenuModel; command_id: Integer;
      key_code, shift_pressed, ctrl_pressed, alt_pressed: PInteger)
      : Integer; stdcall;

    // Retrieves the keyboard accelerator for the specified |index|. Returns true
    // (1) on success.
    get_accelerator_at: function(self: PCefMenuModel; index: Integer;
      key_code, shift_pressed, ctrl_pressed, alt_pressed: PInteger)
      : Integer; stdcall;
  end;

  // Callback structure used for continuation of custom context menu display.
  TCefRunContextMenuCallback = record
    // Base structure.
    base: TCefBase;
    // Complete context menu display by selecting the specified |command_id| and
    // |event_flags|.
    cont: procedure(self: PCefRunContextMenuCallback; command_id: Integer;
      event_flags: TCefEventFlags); stdcall;
    // Cancel context menu display.
    cancel: procedure(self: PCefRunContextMenuCallback); stdcall;
  end;

  // Implement this structure to handle context menu events. The functions of this
  // structure will be called on the UI thread.
  TCefContextMenuHandler = record
    // Base structure.
    base: TCefBase;

    // Called before a context menu is displayed. |params| provides information
    // about the context menu state. |model| initially contains the default
    // context menu. The |model| can be cleared to show no context menu or
    // modified to show a custom menu. Do not keep references to |params| or
    // |model| outside of this callback.
    on_before_context_menu: procedure(self: PCefContextMenuHandler;
      browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams;
      model: PCefMenuModel); stdcall;

    // Called to allow custom display of the context menu. |params| provides
    // information about the context menu state. |model| contains the context menu
    // model resulting from OnBeforeContextMenu. For custom display return true
    // (1) and execute |callback| either synchronously or asynchronously with the
    // selected command ID. For default display return false (0). Do not keep
    // references to |params| or |model| outside of this callback.
    run_context_menu: function(self: PCefContextMenuHandler;
      browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams;
      model: PCefMenuModel; callback: PCefRunContextMenuCallback): Integer; stdcall;

    // Called to execute a command selected from the context menu. Return true (1)
    // if the command was handled or false (0) for the default implementation. See
    // cef_menu_id_t for the command ids that have default implementations. All
    // user-defined command ids should be between MENU_ID_USER_FIRST and
    // MENU_ID_USER_LAST. |params| will have the same values as what was passed to
    // on_before_context_menu(). Do not keep a reference to |params| outside of
    // this callback.
    on_context_menu_command: function(self: PCefContextMenuHandler;
      browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams;
      command_id: Integer; event_flags: Integer): Integer; stdcall;

    // Called when the context menu is dismissed irregardless of whether the menu
    // was NULL or a command was selected.
    on_context_menu_dismissed: procedure(self: PCefContextMenuHandler;
      browser: PCefBrowser; frame: PCefFrame); stdcall;
  end;

  // Provides information about the context menu state. The ethods of this
  // structure can only be accessed on browser process the UI thread.
  TCefContextMenuParams = record
    // Base structure.
    base: TCefBase;

    // Returns the X coordinate of the mouse where the context menu was invoked.
    // Coords are relative to the associated RenderView's origin.
    get_xcoord: function(self: PCefContextMenuParams): Integer; stdcall;

    // Returns the Y coordinate of the mouse where the context menu was invoked.
    // Coords are relative to the associated RenderView's origin.
    get_ycoord: function(self: PCefContextMenuParams): Integer; stdcall;

    // Returns flags representing the type of node that the context menu was
    // invoked on.
    get_type_flags: function(self: PCefContextMenuParams): Integer; stdcall;

    // Returns the URL of the link, if any, that encloses the node that the
    // context menu was invoked on.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_url: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns the link URL, if any, to be used ONLY for "copy link address". We
    // don't validate this field in the frontend process.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_unfiltered_link_url: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns the source URL, if any, for the element that the context menu was
    // invoked on. Example of elements with source URLs are img, audio, and video.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_source_url: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns true (1) if the context menu was invoked on an image which has non-
    // NULL contents.
    has_image_contents: function(self: PCefContextMenuParams): Integer; stdcall;

    // Returns the URL of the top level page that the context menu was invoked on.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_page_url: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns the URL of the subframe that the context menu was invoked on.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_frame_url: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns the character encoding of the subframe that the context menu was
    // invoked on.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_frame_charset: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns the type of context node that the context menu was invoked on.
    get_media_type: function(self: PCefContextMenuParams)
      : TCefContextMenuMediaType; stdcall;

    // Returns flags representing the actions supported by the media element, if
    // any, that the context menu was invoked on.
    get_media_state_flags: function(self: PCefContextMenuParams)
      : Integer; stdcall;

    // Returns the text of the selection, if any, that the context menu was
    // invoked on.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_selection_text: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns the text of the misspelled word, if any, that the context menu was
    // invoked on.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_misspelled_word: function(self: PCefContextMenuParams)
      : PCefStringUserFree; stdcall;

    // Returns true (1) if suggestions exist, false (0) otherwise. Fills in
    // |suggestions| from the spell check service for the misspelled word if there
    // is one.
    get_dictionary_suggestions: function(self: PCefContextMenuParams;
      suggestions: TCefStringList): Integer; stdcall;

    // Returns true (1) if the context menu was invoked on an editable node.
    is_editable: function(self: PCefContextMenuParams): Integer; stdcall;

    // Returns true (1) if the context menu was invoked on an editable node where
    // spell-check is enabled.
    is_spell_check_enabled: function(self: PCefContextMenuParams)
      : Integer; stdcall;

    // Returns flags representing the actions supported by the editable node, if
    // any, that the context menu was invoked on.
    get_edit_state_flags: function(self: PCefContextMenuParams)
      : Integer; stdcall;

    // Returns true (1) if the context menu contains items specified by the
    // renderer process (for example, plugin placeholder or pepper plugin menu
    // items).
    is_custom_menu: function(self: PCefContextMenuParams): Integer; stdcall;

    // Returns true (1) if the context menu was invoked from a pepper plugin.
    is_pepper_menu: function(self: PCefContextMenuParams): Integer; stdcall;
  end;

  // Callback structure used for asynchronous continuation of geolocation
  // permission requests.
  TCefGeolocationCallback = record
    // Base structure.
    base: TCefBase;

    // Call to allow or deny geolocation access.
    cont: procedure(self: PCefGeolocationCallback; allow: Integer); stdcall;
  end;

  // Implement this structure to handle events related to geolocation permission
  // requests. The functions of this structure will be called on the browser
  // process UI thread.
  TCefGeolocationHandler = record
    // Base structure.
    base: TCefBase;

    // Called when a page requests permission to access geolocation information.
    // |requesting_url| is the URL requesting permission and |request_id| is the
    // unique ID for the permission request. Return true (1) and call
    // cef_geolocation_callback_t::cont() either in this function or at a later
    // time to continue or cancel the request. Return false (0) to cancel the
    // request immediately.
    on_request_geolocation_permission: function(self: PCefGeolocationHandler;
      browser: PCefBrowser; const requesting_url: PCefString;
      request_id: Integer; callback: PCefGeolocationCallback): Integer; stdcall;

    // Called when a geolocation access request is canceled. |request_id| is the
    // unique ID for the permission request.
    on_cancel_geolocation_permission: procedure(self: PCefGeolocationHandler;
      browser: PCefBrowser; request_id: Integer); stdcall;
  end;

  // Implement this structure to handle events related to find results. The
  // functions of this structure will be called on the UI thread.
  TCefFindHandler = record
    // Base structure.
    base: TCefBase;

    // Called to report find results returned by cef_browser_host_t::find().
    // |identifer| is the identifier passed to find(), |count| is the number of
    // matches currently identified, |selectionRect| is the location of where the
    // match was found (in window coordinates), |activeMatchOrdinal| is the
    // current position in the search results, and |finalUpdate| is true (1) if
    // this is the last find notification.
    on_find_result: procedure(self: PCefFindHandler; browser: PCefBrowser;
      identifier, count: Integer; const selection_rect: PCefRect;
      active_match_ordinal, final_update: Integer); stdcall;
  end;

  // Implement this structure to provide handler implementations.
  TCefClient = record
    // Base structure.
    base: TCefBase;

    // Return the handler for context menus. If no handler is provided the default
    // implementation will be used.
    get_context_menu_handler: function(self: PCefClient)
      : PCefContextMenuHandler; stdcall;

    // Return the handler for dialogs. If no handler is provided the default
    // implementation will be used.
    get_dialog_handler: function(self: PCefClient): PCefDialogHandler; stdcall;

    // Return the handler for browser display state events.
    get_display_handler: function(self: PCefClient)
      : PCefDisplayHandler; stdcall;

    // Return the handler for download events. If no handler is returned downloads
    // will not be allowed.
    get_download_handler: function(self: PCefClient)
      : PCefDownloadHandler; stdcall;

    // Return the handler for drag events.
    get_drag_handler: function(self: PCefClient): PCefDragHandler; stdcall;

    // Return the handler for find result events.
    get_find_handler: function(self: PCefClient): PCefFindHandler; stdcall;

    // Return the handler for focus events.
    get_focus_handler: function(self: PCefClient): PCefFocusHandler; stdcall;

    // Return the handler for geolocation permissions requests. If no handler is
    // provided geolocation access will be denied by default.
    get_geolocation_handler: function(self: PCefClient)
      : PCefGeolocationHandler; stdcall;

    // Return the handler for JavaScript dialog events.
    get_jsdialog_handler: function(self: PCefClient)
      : PCefJsDialogHandler; stdcall;

    // Return the handler for keyboard events.
    get_keyboard_handler: function(self: PCefClient)
      : PCefKeyboardHandler; stdcall;

    // Return the handler for browser life span events.
    get_life_span_handler: function(self: PCefClient)
      : PCefLifeSpanHandler; stdcall;

    // Return the handler for browser load status events.
    get_load_handler: function(self: PCefClient): PCefLoadHandler; stdcall;

    // Return the handler for off-screen rendering events.
    get_render_handler: function(self: PCefClient): PCefRenderHandler; stdcall;

    // Return the handler for browser request events.
    get_request_handler: function(self: PCefClient)
      : PCefRequestHandler; stdcall;

    // Called when a new message is received from a different process. Return true
    // (1) if the message was handled or false (0) otherwise. Do not keep a
    // reference to or attempt to access the message outside of this callback.
    on_process_message_received: function(self: PCefClient;
      browser: PCefBrowser; source_process: TCefProcessId;
      message: PCefProcessMessage): Integer; stdcall;
  end;

  // Structure used to represent a web request. The functions of this structure
  // may be called on any thread.
  TCefRequest = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is read-only.
    is_read_only: function(self: PCefRequest): Integer; stdcall;

    // Get the fully qualified URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_url: function(self: PCefRequest): PCefStringUserFree; stdcall;
    // Set the fully qualified URL.
    set_url: procedure(self: PCefRequest; const url: PCefString); stdcall;

    // Get the request function type. The value will default to POST if post data
    // is provided and GET otherwise.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_method: function(self: PCefRequest): PCefStringUserFree; stdcall;
    // Set the request function type.
    set_method: procedure(self: PCefRequest; const method: PCefString); stdcall;

    // Set the referrer URL and policy. If non-NULL the referrer URL must be fully
    // qualified with an HTTP or HTTPS scheme component. Any username, password or
    // ref component will be removed.
    set_referrer: procedure(self: PCefRequest; const referrer_url: PCefString;
      policy: TCefReferrerPolicy); stdcall;
    // Get the referrer URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_referrer_url: function(self: PCefRequest): PCefStringUserFree; stdcall;
    // Get the referrer policy.
    get_referrer_policy: function(self: PCefRequest): TCefReferrerPolicy; stdcall;

    // Get the post data.
    get_post_data: function(self: PCefRequest): PCefPostData; stdcall;
    // Set the post data.
    set_post_data: procedure(self: PCefRequest;
      postData: PCefPostData); stdcall;

    // Get the header values. Will not include the Referer value if any.
    get_header_map: procedure(self: PCefRequest; headerMap: TCefStringMultimap); stdcall;
    // Set the header values. If a Referer value exists in the header map it will
    // be removed and ignored.
    set_header_map: procedure(self: PCefRequest;
      headerMap: TCefStringMultimap); stdcall;

    // Set all values at one time.
    set_: procedure(self: PCefRequest; const url, method: PCefString;
      postData: PCefPostData; headerMap: TCefStringMultimap); stdcall;

    // Get the flags used in combination with cef_urlrequest_t. See
    // cef_urlrequest_flags_t for supported values.
    get_flags: function(self: PCefRequest): Integer; stdcall;
    // Set the flags used in combination with cef_urlrequest_t.  See
    // cef_urlrequest_flags_t for supported values.
    set_flags: procedure(self: PCefRequest; flags: Integer); stdcall;

    // Get the URL to the first party for cookies used in combination with
    // cef_urlrequest_t.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_first_party_for_cookies: function(self: PCefRequest)
      : PCefStringUserFree; stdcall;
    // Set the URL to the first party for cookies used in combination with
    // cef_urlrequest_t.
    set_first_party_for_cookies: procedure(self: PCefRequest;
      const url: PCefString); stdcall;

    // Get the resource type for this request. Only available in the browser
    // process.
    get_resource_type: function(self: PCefRequest): TCefResourceType; stdcall;

    // Get the transition type for this request. Only available in the browser
    // process and only applies to requests that represent a main frame or sub-
    // frame navigation.
    get_transition_type: function(self: PCefRequest)
      : TCefTransitionType; stdcall;

    // Returns the globally unique identifier for this request or 0 if not
    // specified. Can be used by cef_request_tHandler implementations in the
    // browser process to track a single request across multiple callbacks.
    get_identifier: function(self: PCefRequest): UInt64; stdcall;
  end;

  TCefPostDataElementArray = array
    [0 .. (High(Integer) div SizeOf(PCefPostDataElement)) - 1]
    of PCefPostDataElement;
  PCefPostDataElementArray = ^TCefPostDataElementArray;

  // Structure used to represent post data for a web request. The functions of
  // this structure may be called on any thread.
  TCefPostData = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is read-only.
    is_read_only: function(self: PCefPostData): Integer; stdcall;

    // Returns true (1) if the underlying POST data includes elements that are not
    // represented by this cef_post_data_t object (for example, multi-part file
    // upload data). Modifying cef_post_data_t objects with excluded elements may
    // result in the request failing.
    has_excluded_elements: function(self: PCefPostData): Integer; stdcall;

    // Returns the number of existing post data elements.
    get_element_count: function(self: PCefPostData): NativeUInt; stdcall;

    // Retrieve the post data elements.
    get_elements: procedure(self: PCefPostData; elementsCount: PNativeUInt;
      elements: PCefPostDataElementArray); stdcall;

    // Remove the specified post data element.  Returns true (1) if the removal
    // succeeds.
    remove_element: function(self: PCefPostData; element: PCefPostDataElement)
      : Integer; stdcall;

    // Add the specified post data element.  Returns true (1) if the add succeeds.
    add_element: function(self: PCefPostData; element: PCefPostDataElement)
      : Integer; stdcall;

    // Remove all existing post data elements.
    remove_elements: procedure(self: PCefPostData); stdcall;

  end;

  // Structure used to represent a single element in the request post data. The
  // functions of this structure may be called on any thread.
  TCefPostDataElement = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is read-only.
    is_read_only: function(self: PCefPostDataElement): Integer; stdcall;

    // Remove all contents from the post data element.
    set_to_empty: procedure(self: PCefPostDataElement); stdcall;

    // The post data element will represent a file.
    set_to_file: procedure(self: PCefPostDataElement;
      const fileName: PCefString); stdcall;

    // The post data element will represent bytes.  The bytes passed in will be
    // copied.
    set_to_bytes: procedure(self: PCefPostDataElement; size: NativeUInt;
      const bytes: Pointer); stdcall;

    // Return the type of this post data element.
    get_type: function(self: PCefPostDataElement)
      : TCefPostDataElementType; stdcall;

    // Return the file name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_file: function(self: PCefPostDataElement): PCefStringUserFree; stdcall;

    // Return the number of bytes.
    get_bytes_count: function(self: PCefPostDataElement): NativeUInt; stdcall;

    // Read up to |size| bytes into |bytes| and return the number of bytes
    // actually read.
    get_bytes: function(self: PCefPostDataElement; size: NativeUInt;
      bytes: Pointer): NativeUInt; stdcall;
  end;

  // Structure used to represent a web response. The functions of this structure
  // may be called on any thread.
  TCefResponse = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is read-only.
    is_read_only: function(self: PCefResponse): Integer; stdcall;

    // Get the response status code.
    get_status: function(self: PCefResponse): Integer; stdcall;
    // Set the response status code.
    set_status: procedure(self: PCefResponse; status: Integer); stdcall;

    // Get the response status text.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_status_text: function(self: PCefResponse): PCefStringUserFree; stdcall;
    // Set the response status text.
    set_status_text: procedure(self: PCefResponse;
      const statusText: PCefString); stdcall;

    // Get the response mime type.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_mime_type: function(self: PCefResponse): PCefStringUserFree; stdcall;
    // Set the response mime type.
    set_mime_type: procedure(self: PCefResponse;
      const mimeType: PCefString); stdcall;

    // Get the value for the specified response header field.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_header: function(self: PCefResponse; const name: PCefString)
      : PCefStringUserFree; stdcall;

    // Get all response header fields.
    get_header_map: procedure(self: PCefResponse;
      headerMap: TCefStringMultimap); stdcall;
    // Set all response header fields.
    set_header_map: procedure(self: PCefResponse;
      headerMap: TCefStringMultimap); stdcall;
  end;

  // Structure the client can implement to provide a custom stream reader. The
  // functions of this structure may be called on any thread.
  TCefReadHandler = record
    // Base structure.
    base: TCefBase;

    // Read raw binary data.
    read: function(self: PCefReadHandler; ptr: Pointer; size, n: NativeUInt)
      : NativeUInt; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
    seek: function(self: PCefReadHandler; offset: Int64; whence: Integer)
      : Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefReadHandler): Int64; stdcall;

    // Return non-zero if at end of file.
    eof: function(self: PCefReadHandler): Integer; stdcall;

    // Return true (1) if this handler performs work like accessing the file
    // system which may block. Used as a hint for determining the thread to access
    // the handler from.
    may_block: function(self: PCefReadHandler): Integer; stdcall;
  end;

  // Structure used to read data from a stream. The functions of this structure
  // may be called on any thread.
  TCefStreamReader = record
    // Base structure.
    base: TCefBase;

    // Read raw binary data.
    read: function(self: PCefStreamReader; ptr: Pointer; size, n: NativeUInt)
      : NativeUInt; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
    seek: function(self: PCefStreamReader; offset: Int64; whence: Integer)
      : Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefStreamReader): Int64; stdcall;

    // Return non-zero if at end of file.
    eof: function(self: PCefStreamReader): Integer; stdcall;

    // Returns true (1) if this reader performs work like accessing the file
    // system which may block. Used as a hint for determining the thread to access
    // the reader from.
    may_block: function(self: PCefStreamReader): Integer; stdcall;
  end;

  // Structure the client can implement to provide a custom stream writer. The
  // functions of this structure may be called on any thread.
  TCefWriteHandler = record
    // Base structure.
    base: TCefBase;

    // Write raw binary data.
    write: function(self:PCefWriteHandler; const ptr:Pointer;
      size, n:NativeUInt): NativeUInt; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET.
    seek: function(self: PCefWriteHandler; offset: Int64; whence: Integer)
      : Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefWriteHandler): Int64; stdcall;

    // Flush the stream.
    flush: function(self: PCefWriteHandler): Integer; stdcall;

    // Return true (1) if this handler performs work like accessing the file
    // system which may block. Used as a hint for determining the thread to access
    // the handler from.
    may_block: function(self: PCefWriteHandler): Integer; stdcall;
  end;

  // Structure used to write data to a stream. The functions of this structure may
  // be called on any thread.
  // TODO: Implement class
  TCefStreamWriter = record
    // Base structure.
    base: TCefBase;

    // Write raw binary data.
    write: function(self:PCefStreamWriter; const ptr:Pointer;
      size, n:NativeUInt): NativeUInt; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET.
    seek: function(self: PCefStreamWriter; offset: Int64; whence: Integer)
      : Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefStreamWriter): Int64; stdcall;

    // Flush the stream.
    flush: function(self: PCefStreamWriter): Integer; stdcall;

    // Returns true (1) if this writer performs work like accessing the file
    // system which may block. Used as a hint for determining the thread to access
    // the writer from.
    may_block: function(self: PCefStreamWriter): Integer; stdcall;
  end;

  // Structure representing a V8 context handle. V8 handles can only be accessed
  // from the thread on which they are created. Valid threads for creating a V8
  // handle include the render process main thread (TID_RENDERER) and WebWorker
  // threads. A task runner for posting tasks on the associated thread can be
  // retrieved via the cef_v8context_t::get_task_runner() function.

  TCefv8Context = record
    // Base structure.
    base: TCefBase;

    // Returns the task runner associated with this context. V8 handles can only
    // be accessed from the thread on which they are created. This function can be
    // called on any render process thread.
    get_task_runner: function(self: PCefv8Context): PCefTask; stdcall;

    // Returns true (1) if the underlying handle is valid and it can be accessed
    // on the current thread. Do not call any other functions if this function
    // returns false (0).
    is_valid: function(self: PCefv8Context): Integer; stdcall;

    // Returns the browser for this context. This function will return an NULL
    // reference for WebWorker contexts.
    get_browser: function(self: PCefv8Context): PCefBrowser; stdcall;

    // Returns the frame for this context. This function will return an NULL
    // reference for WebWorker contexts.
    get_frame: function(self: PCefv8Context): PCefFrame; stdcall;

    // Returns the global object for this context. The context must be entered
    // before calling this function.
    get_global: function(self: PCefv8Context): PCefv8Value; stdcall;

    // Enter this context. A context must be explicitly entered before creating a
    // V8 Object, Array, Function or Date asynchronously. exit() must be called
    // the same number of times as enter() before releasing this context. V8
    // objects belong to the context in which they are created. Returns true (1)
    // if the scope was entered successfully.
    enter: function(self: PCefv8Context): Integer; stdcall;

    // Exit this context. Call this function only after calling enter(). Returns
    // true (1) if the scope was exited successfully.
    exit: function(self: PCefv8Context): Integer; stdcall;

    // Returns true (1) if this object is pointing to the same handle as |that|
    // object.
    is_same: function(self, that: PCefv8Context): Integer; stdcall;

    // Evaluates the specified JavaScript code using this context's global object.
    // On success |retval| will be set to the return value, if any, and the
    // function will return true (1). On failure |exception| will be set to the
    // exception, if any, and the function will return false (0).
    eval: function(self: PCefv8Context; const code: PCefString;
      var retval: PCefv8Value; var exception: PCefV8Exception)
      : Integer; stdcall;
  end;

  // Structure that should be implemented to handle V8 function calls. The
  // functions of this structure will be called on the thread associated with the
  // V8 function.
  TCefv8Handler = record
    // Base structure.
    base: TCefBase;

    // Handle execution of the function identified by |name|. |object| is the
    // receiver ('this' object) of the function. |arguments| is the list of
    // arguments passed to the function. If execution succeeds set |retval| to the
    // function return value. If execution fails set |exception| to the exception
    // that will be thrown. Return true (1) if execution was handled.
    execute: function(self: PCefv8Handler; const name: PCefString;
      obj: PCefv8Value; argumentsCount: NativeUInt;
      const arguments: PPCefV8Value; var retval: PCefv8Value;
      var exception: TCefString): Integer; stdcall;
  end;

  // Structure that should be implemented to handle V8 accessor calls. Accessor
  // identifiers are registered by calling cef_v8value_t::set_value(). The
  // functions of this structure will be called on the thread associated with the
  // V8 accessor.
  TCefV8Accessor = record
    // Base structure.
    base: TCefBase;

    // Handle retrieval the accessor value identified by |name|. |object| is the
    // receiver ('this' object) of the accessor. If retrieval succeeds set
    // |retval| to the return value. If retrieval fails set |exception| to the
    // exception that will be thrown. Return true (1) if accessor retrieval was
    // handled.
    get: function(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; out retval: PCefv8Value; exception: PCefString)
      : Integer; stdcall;

    // Handle assignment of the accessor value identified by |name|. |object| is
    // the receiver ('this' object) of the accessor. |value| is the new value
    // being assigned to the accessor. If assignment fails set |exception| to the
    // exception that will be thrown. Return true (1) if accessor assignment was
    // handled.
    put: function(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; value: PCefv8Value; exception: PCefString)
      : Integer; stdcall;
  end;

  // Structure representing a V8 exception. The functions of this structure may be
  // called on any render process thread.
  TCefV8Exception = record
    // Base structure.
    base: TCefBase;

    // Returns the exception message.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_message: function(self: PCefV8Exception): PCefStringUserFree; stdcall;

    // Returns the line of source code that the exception occurred within.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_source_line: function(self: PCefV8Exception)
      : PCefStringUserFree; stdcall;

    // Returns the resource name for the script from where the function causing
    // the error originates.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_script_resource_name: function(self: PCefV8Exception)
      : PCefStringUserFree; stdcall;

    // Returns the 1-based number of the line where the error occurred or 0 if the
    // line number is unknown.
    get_line_number: function(self: PCefV8Exception): Integer; stdcall;

    // Returns the index within the script of the first character where the error
    // occurred.
    get_start_position: function(self: PCefV8Exception): Integer; stdcall;

    // Returns the index within the script of the last character where the error
    // occurred.
    get_end_position: function(self: PCefV8Exception): Integer; stdcall;

    // Returns the index within the line of the first character where the error
    // occurred.
    get_start_column: function(self: PCefV8Exception): Integer; stdcall;

    // Returns the index within the line of the last character where the error
    // occurred.
    get_end_column: function(self: PCefV8Exception): Integer; stdcall;
  end;


  // Structure representing a V8 value handle. V8 handles can only be accessed
  // from the thread on which they are created. Valid threads for creating a V8
  // handle include the render process main thread (TID_RENDERER) and WebWorker
  // threads. A task runner for posting tasks on the associated thread can be
  // retrieved via the cef_v8context_t::get_task_runner() function.

  TCefv8Value = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if the underlying handle is valid and it can be accessed
    // on the current thread. Do not call any other functions if this function
    // returns false (0).
    is_valid: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is undefined.
    is_undefined: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is null.
    is_null: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is bool.
    is_bool: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is int.
    is_int: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is unsigned int.
    is_uint: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is double.
    is_double: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is Date.
    is_date: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is string.
    is_string: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is object.
    is_object: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is array.
    is_array: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is function.
    is_function: function(self: PCefv8Value): Integer; stdcall;

    // Returns true (1) if this object is pointing to the same handle as |that|
    // object.
    is_same: function(self, that: PCefv8Value): Integer; stdcall;

    // Return a bool value.  The underlying data will be converted to if
    // necessary.
    get_bool_value: function(self: PCefv8Value): Integer; stdcall;
    // Return an int value.  The underlying data will be converted to if
    // necessary.
    get_int_value: function(self: PCefv8Value): Integer; stdcall;
    // Return an unisgned int value.  The underlying data will be converted to if
    // necessary.
    get_uint_value: function(self: PCefv8Value): Cardinal; stdcall;
    // Return a double value.  The underlying data will be converted to if
    // necessary.
    get_double_value: function(self: PCefv8Value): Double; stdcall;
    // Return a Date value.  The underlying data will be converted to if
    // necessary.
    get_date_value: function(self: PCefv8Value): TCefTime; stdcall;
    // Return a string value.  The underlying data will be converted to if
    // necessary.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_string_value: function(self: PCefv8Value): PCefStringUserFree; stdcall;


    // OBJECT METHODS - These functions are only available on objects. Arrays and
    // functions are also objects. String- and integer-based keys can be used
    // interchangably with the framework converting between them as necessary.

    // Returns true (1) if this is a user created object.
    is_user_created: function(self: PCefv8Value): Integer; stdcall;

    // Returns true (1) if the last function call resulted in an exception. This
    // attribute exists only in the scope of the current CEF value object.
    has_exception: function(self: PCefv8Value): Integer; stdcall;

    // Returns the exception resulting from the last function call. This attribute
    // exists only in the scope of the current CEF value object.
    get_exception: function(self: PCefv8Value): PCefV8Exception; stdcall;

    // Clears the last exception and returns true (1) on success.
    clear_exception: function(self: PCefv8Value): Integer; stdcall;

    // Returns true (1) if this object will re-throw future exceptions. This
    // attribute exists only in the scope of the current CEF value object.
    will_rethrow_exceptions: function(self: PCefv8Value): Integer; stdcall;

    // Set whether this object will re-throw future exceptions. By default
    // exceptions are not re-thrown. If a exception is re-thrown the current
    // context should not be accessed again until after the exception has been
    // caught and not re-thrown. Returns true (1) on success. This attribute
    // exists only in the scope of the current CEF value object.
    set_rethrow_exceptions: function(self: PCefv8Value; rethrow: Integer)
      : Integer; stdcall;

    // Returns true (1) if the object has a value with the specified identifier.
    has_value_bykey: function(self: PCefv8Value; const key: PCefString)
      : Integer; stdcall;
    // Returns true (1) if the object has a value with the specified identifier.
    has_value_byindex: function(self: PCefv8Value; index: Integer)
      : Integer; stdcall;

    // Deletes the value with the specified identifier and returns true (1) on
    // success. Returns false (0) if this function is called incorrectly or an
    // exception is thrown. For read-only and don't-delete values this function
    // will return true (1) even though deletion failed.
    delete_value_bykey: function(self: PCefv8Value; const key: PCefString)
      : Integer; stdcall;
    // Deletes the value with the specified identifier and returns true (1) on
    // success. Returns false (0) if this function is called incorrectly, deletion
    // fails or an exception is thrown. For read-only and don't-delete values this
    // function will return true (1) even though deletion failed.
    delete_value_byindex: function(self: PCefv8Value; index: Integer)
      : Integer; stdcall;

    // Returns the value with the specified identifier on success. Returns NULL if
    // this function is called incorrectly or an exception is thrown.
    get_value_bykey: function(self: PCefv8Value; const key: PCefString)
      : PCefv8Value; stdcall;
    // Returns the value with the specified identifier on success. Returns NULL if
    // this function is called incorrectly or an exception is thrown.
    get_value_byindex: function(self: PCefv8Value; index: Integer)
      : PCefv8Value; stdcall;

    // Associates a value with the specified identifier and returns true (1) on
    // success. Returns false (0) if this function is called incorrectly or an
    // exception is thrown. For read-only values this function will return true
    // (1) even though assignment failed.
    set_value_bykey: function(self: PCefv8Value; const key: PCefString;
      value: PCefv8Value; attribute: Integer): Integer; stdcall;
    // Associates a value with the specified identifier and returns true (1) on
    // success. Returns false (0) if this function is called incorrectly or an
    // exception is thrown. For read-only values this function will return true
    // (1) even though assignment failed.
    set_value_byindex: function(self: PCefv8Value; index: Integer;
      value: PCefv8Value): Integer; stdcall;

    // Registers an identifier and returns true (1) on success. Access to the
    // identifier will be forwarded to the cef_v8accessor_t instance passed to
    // cef_v8value_t::cef_v8value_create_object(). Returns false (0) if this
    // function is called incorrectly or an exception is thrown. For read-only
    // values this function will return true (1) even though assignment failed.
    set_value_byaccessor: function(self: PCefv8Value; const key: PCefString;
      settings: Integer; attribute: Integer): Integer; stdcall;

    // Read the keys for the object's values into the specified vector. Integer-
    // based keys will also be returned as strings.
    get_keys: function(self: PCefv8Value; keys: TCefStringList)
      : Integer; stdcall;

    // Sets the user data for this object and returns true (1) on success. Returns
    // false (0) if this function is called incorrectly. This function can only be
    // called on user created objects.
    set_user_data: function(self: PCefv8Value; user_data: PCefBase)
      : Integer; stdcall;

    // Returns the user data, if any, assigned to this object.
    get_user_data: function(self: PCefv8Value): PCefBase; stdcall;

    // Returns the amount of externally allocated memory registered for the
    // object.
    get_externally_allocated_memory: function(self: PCefv8Value)
      : Integer; stdcall;

    // Adjusts the amount of registered external memory for the object. Used to
    // give V8 an indication of the amount of externally allocated memory that is
    // kept alive by JavaScript objects. V8 uses this information to decide when
    // to perform global garbage collection. Each cef_v8value_t tracks the amount
    // of external memory associated with it and automatically decreases the
    // global total by the appropriate amount on its destruction.
    // |change_in_bytes| specifies the number of bytes to adjust by. This function
    // returns the number of bytes associated with the object after the
    // adjustment. This function can only be called on user created objects.
    adjust_externally_allocated_memory: function(self: PCefv8Value;
      change_in_bytes: Integer): Integer; stdcall;

    // ARRAY METHODS - These functions are only available on arrays.

    // Returns the number of elements in the array.
    get_array_length: function(self: PCefv8Value): Integer; stdcall;


    // FUNCTION METHODS - These functions are only available on functions.

    // Returns the function name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_function_name: function(self: PCefv8Value): PCefStringUserFree; stdcall;

    // Returns the function handler or NULL if not a CEF-created function.
    get_function_handler: function(self: PCefv8Value): PCefv8Handler; stdcall;

    // Execute the function using the current V8 context. This function should
    // only be called from within the scope of a cef_v8handler_t or
    // cef_v8accessor_t callback, or in combination with calling enter() and
    // exit() on a stored cef_v8context_t reference. |object| is the receiver
    // ('this' object) of the function. If |object| is NULL the current context's
    // global object will be used. |arguments| is the list of arguments that will
    // be passed to the function. Returns the function return value on success.
    // Returns NULL if this function is called incorrectly or an exception is
    // thrown.
    execute_function: function(self: PCefv8Value; obj: PCefv8Value;
      argumentsCount: NativeUInt; const arguments: PPCefV8Value)
      : PCefv8Value; stdcall;

    // Execute the function using the specified V8 context. |object| is the
    // receiver ('this' object) of the function. If |object| is NULL the specified
    // context's global object will be used. |arguments| is the list of arguments
    // that will be passed to the function. Returns the function return value on
    // success. Returns NULL if this function is called incorrectly or an
    // exception is thrown.
    execute_function_with_context: function(self: PCefv8Value;
      context: PCefv8Context; obj: PCefv8Value; argumentsCount: NativeUInt;
      const arguments: PPCefV8Value): PCefv8Value; stdcall;
  end;

  // Structure representing a V8 stack trace handle. V8 handles can only be
  // accessed from the thread on which they are created. Valid threads for
  // creating a V8 handle include the render process main thread (TID_RENDERER)
  // and WebWorker threads. A task runner for posting tasks on the associated
  // thread can be retrieved via the cef_v8context_t::get_task_runner() function.
  TCefV8StackTrace = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if the underlying handle is valid and it can be accessed
    // on the current thread. Do not call any other functions if this function
    // returns false (0).
    is_valid: function(self: PCefV8StackTrace): Integer; stdcall;

    // Returns the number of stack frames.
    get_frame_count: function(self: PCefV8StackTrace): Integer; stdcall;

    // Returns the stack frame at the specified 0-based index.
    get_frame: function(self: PCefV8StackTrace; index: Integer)
      : PCefV8StackFrame; stdcall;
  end;

  // Structure representing a V8 stack frame handle. V8 handles can only be
  // accessed from the thread on which they are created. Valid threads for
  // creating a V8 handle include the render process main thread (TID_RENDERER)
  // and WebWorker threads. A task runner for posting tasks on the associated
  // thread can be retrieved via the cef_v8context_t::get_task_runner() function.

  TCefV8StackFrame = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if the underlying handle is valid and it can be accessed
    // on the current thread. Do not call any other functions if this function
    // returns false (0).
    is_valid: function(self: PCefV8StackFrame): Integer; stdcall;

    // Returns the name of the resource script that contains the function.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_script_name: function(self: PCefV8StackFrame)
      : PCefStringUserFree; stdcall;

    // Returns the name of the resource script that contains the function or the
    // sourceURL value if the script name is undefined and its source ends with a
    // "//@ sourceURL=..." string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_script_name_or_source_url: function(self: PCefV8StackFrame)
      : PCefStringUserFree; stdcall;

    // Returns the name of the function.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_function_name: function(self: PCefV8StackFrame)
      : PCefStringUserFree; stdcall;

    // Returns the 1-based line number for the function call or 0 if unknown.
    get_line_number: function(self: PCefV8StackFrame): Integer; stdcall;

    // Returns the 1-based column offset on the line for the function call or 0 if
    // unknown.
    get_column: function(self: PCefV8StackFrame): Integer; stdcall;

    // Returns true (1) if the function was compiled using eval().
    is_eval: function(self: PCefV8StackFrame): Integer; stdcall;

    // Returns true (1) if the function was called as a constructor via "new".
    is_constructor: function(self: PCefV8StackFrame): Integer; stdcall;
  end;

  // Structure that manages custom scheme registrations.
  TCefSchemeRegistrar = record
    // Base structure.
    base: TCefBase;

    // Register a custom scheme. This function should not be called for the built-
    // in HTTP, HTTPS, FILE, FTP, ABOUT and DATA schemes.
    //
    // If |is_standard| is true (1) the scheme will be treated as a standard
    // scheme. Standard schemes are subject to URL canonicalization and parsing
    // rules as defined in the Common Internet Scheme Syntax RFC 1738 Section 3.1
    // available at http://www.ietf.org/rfc/rfc1738.txt
    //
    // In particular, the syntax for standard scheme URLs must be of the form:
    // <pre>
    // [scheme]://[username]:[password]@[host]:[port]/[url-path]
    // </pre> Standard scheme URLs must have a host component that is a fully
    // qualified domain name as defined in Section 3.5 of RFC 1034 [13] and
    // Section 2.1 of RFC 1123. These URLs will be canonicalized to
    // "scheme://host/path" in the simplest case and
    // "scheme://username:password@host:port/path" in the most explicit case. For
    // example, "scheme:host/path" and "scheme:///host/path" will both be
    // canonicalized to "scheme://host/path". The origin of a standard scheme URL
    // is the combination of scheme, host and port (i.e., "scheme://host:port" in
    // the most explicit case).
    //
    // For non-standard scheme URLs only the "scheme:" component is parsed and
    // canonicalized. The remainder of the URL will be passed to the handler as-
    // is. For example, "scheme:///some%20text" will remain the same. Non-standard
    // scheme URLs cannot be used as a target for form submission.
    //
    // If |is_local| is true (1) the scheme will be treated as local (i.e., with
    // the same security rules as those applied to "file" URLs). Normal pages
    // cannot link to or access local URLs. Also, by default, local URLs can only
    // perform XMLHttpRequest calls to the same URL (origin + path) that
    // originated the request. To allow XMLHttpRequest calls from a local URL to
    // other URLs with the same origin set the
    // CefSettings.file_access_from_file_urls_allowed value to true (1). To allow
    // XMLHttpRequest calls from a local URL to all origins set the
    // CefSettings.universal_access_from_file_urls_allowed value to true (1).
    //
    // If |is_display_isolated| is true (1) the scheme will be treated as display-
    // isolated. This means that pages cannot display these URLs unless they are
    // from the same scheme. For example, pages in another origin cannot create
    // iframes or hyperlinks to URLs with this scheme.
    //
    // This function may be called on any thread. It should only be called once
    // per unique |scheme_name| value. If |scheme_name| is already registered or
    // if an error occurs this function will return false (0).
    add_custom_scheme: function(self: PCefSchemeRegistrar;
      const scheme_name: PCefString; is_standard, is_local, is_display_isolated
      : Integer): Integer; stdcall;
  end;

  // Structure that creates cef_scheme_handler_t instances. The functions of this
  // structure will always be called on the IO thread.
  TCefSchemeHandlerFactory = record
    // Base structure.
    base: TCefBase;

    // Return a new resource handler instance to handle the request or an NULL
    // reference to allow default handling of the request. |browser| and |frame|
    // will be the browser window and frame respectively that originated the
    // request or NULL if the request did not originate from a browser window (for
    // example, if the request came from cef_urlrequest_t). The |request| object
    // passed to this function will not contain cookie data.
    create: function(self: PCefSchemeHandlerFactory; browser: PCefBrowser;
      frame: PCefFrame; const scheme_name: PCefString; request: PCefRequest)
      : PCefResourceHandler; stdcall;
  end;

  // Structure used to represent a download item.
  TCefDownloadItem = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. Do not call any other functions
    // if this function returns false (0).
    is_valid: function(self: PCefDownloadItem): Integer; stdcall;

    // Returns true (1) if the download is in progress.
    is_in_progress: function(self: PCefDownloadItem): Integer; stdcall;

    // Returns true (1) if the download is complete.
    is_complete: function(self: PCefDownloadItem): Integer; stdcall;

    // Returns true (1) if the download has been canceled or interrupted.
    is_canceled: function(self: PCefDownloadItem): Integer; stdcall;

    // Returns a simple speed estimate in bytes/s.
    get_current_speed: function(self: PCefDownloadItem): Int64; stdcall;

    // Returns the rough percent complete or -1 if the receive total size is
    // unknown.
    get_percent_complete: function(self: PCefDownloadItem): Integer; stdcall;

    // Returns the total number of bytes.
    get_total_bytes: function(self: PCefDownloadItem): Int64; stdcall;

    // Returns the number of received bytes.
    get_received_bytes: function(self: PCefDownloadItem): Int64; stdcall;

    // Returns the time that the download started.
    get_start_time: function(self: PCefDownloadItem): TCefTime; stdcall;

    // Returns the time that the download ended.
    get_end_time: function(self: PCefDownloadItem): TCefTime; stdcall;

    // Returns the full path to the downloaded or downloading file.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_full_path: function(self: PCefDownloadItem)
      : PCefStringUserFree; stdcall;

    // Returns the unique identifier for this download.
    get_id: function(self: PCefDownloadItem): Cardinal; stdcall;

    // Returns the URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_url: function(self: PCefDownloadItem): PCefStringUserFree; stdcall;

    // Returns the original URL before any redirections.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_original_url: function(self: PCefDownloadItem): PCefStringUserFree; stdcall;

    // Returns the suggested file name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_suggested_file_name: function(self: PCefDownloadItem)
      : PCefStringUserFree; stdcall;

    // Returns the content disposition.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_content_disposition: function(self: PCefDownloadItem)
      : PCefStringUserFree; stdcall;

    // Returns the mime type.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_mime_type: function(self: PCefDownloadItem)
      : PCefStringUserFree; stdcall;
  end;

  // Callback structure used to asynchronously continue a download.
  TCefBeforeDownloadCallback = record
    // Base structure.
    base: TCefBase;

    // Call to continue the download. Set |download_path| to the full file path
    // for the download including the file name or leave blank to use the
    // suggested name and the default temp directory. Set |show_dialog| to true
    // (1) if you do wish to show the default "Save As" dialog.
    cont: procedure(self: PCefBeforeDownloadCallback;
      const download_path: PCefString; show_dialog: Integer); stdcall;
  end;

  // Callback structure used to asynchronously cancel a download.
  TCefDownloadItemCallback = record
    // Base structure.
    base: TCefBase;

    // Call to cancel the download.
    cancel: procedure(self: PCefDownloadItemCallback); stdcall;
    // Call to pause the download.
    pause: procedure(self: PCefDownloadItemCallback); stdcall;
    // Call to resume the download.
    resume: procedure(self: PCefDownloadItemCallback); stdcall;
  end;

  // Structure used to handle file downloads. The functions of this structure will
  // always be called on the UI thread.
  TCefDownloadHandler = record
    // Base structure.
    base: TCefBase;

    // Called before a download begins. |suggested_name| is the suggested name for
    // the download file. By default the download will be canceled. Execute
    // |callback| either asynchronously or in this function to continue the
    // download if desired. Do not keep a reference to |download_item| outside of
    // this function.
    on_before_download: procedure(self: PCefDownloadHandler;
      browser: PCefBrowser; download_item: PCefDownloadItem;
      const suggested_name: PCefString;
      callback: PCefBeforeDownloadCallback); stdcall;

    // Called when a download's status or progress information has been updated.
    // This may be called multiple times before and after on_before_download().
    // Execute |callback| either asynchronously or in this function to cancel the
    // download if desired. Do not keep a reference to |download_item| outside of
    // this function.
    on_download_updated: procedure(self: PCefDownloadHandler;
      browser: PCefBrowser; download_item: PCefDownloadItem;
      callback: PCefDownloadItemCallback); stdcall;
  end;

  // Structure that supports the reading of XML data via the libxml streaming API.
  // The functions of this structure should only be called on the thread that
  // creates the object.
  TCefXmlReader = record
    // Base structure.
    base: TCefBase;

    // Moves the cursor to the next node in the document. This function must be
    // called at least once to set the current cursor position. Returns true (1)
    // if the cursor position was set successfully.
    move_to_next_node: function(self: PCefXmlReader): Integer; stdcall;

    // Close the document. This should be called directly to ensure that cleanup
    // occurs on the correct thread.
    close: function(self: PCefXmlReader): Integer; stdcall;

    // Returns true (1) if an error has been reported by the XML parser.
    has_error: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the error string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_error: function(self: PCefXmlReader): PCefStringUserFree; stdcall;


    // The below functions retrieve data for the node at the current cursor
    // position.

    // Returns the node type.
    get_type: function(self: PCefXmlReader): TCefXmlNodeType; stdcall;

    // Returns the node depth. Depth starts at 0 for the root node.
    get_depth: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
    // LocalPart for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_local_name: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
    // additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_prefix: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the qualified name, equal to (Prefix:)LocalName. See
    // http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_qualified_name: function(self: PCefXmlReader)
      : PCefStringUserFree; stdcall;

    // Returns the URI defining the namespace associated with the node. See
    // http://www.w3.org/TR/REC-xml-names/ for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_namespace_uri: function(self: PCefXmlReader)
      : PCefStringUserFree; stdcall;

    // Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
    // additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_base_uri: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the xml:lang scope within which the node resides. See
    // http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_xml_lang: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns true (1) if the node represents an NULL element. <a/> is considered
    // NULL but <a></a> is not.
    is_empty_element: function(self: PCefXmlReader): Integer; stdcall;

    // Returns true (1) if the node has a text value.
    has_value: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the text value.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_value: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns true (1) if the node has attributes.
    has_attributes: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the number of attributes.
    get_attribute_count: function(self: PCefXmlReader): NativeUInt; stdcall;

    // Returns the value of the attribute at the specified 0-based index.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_attribute_byindex: function(self: PCefXmlReader; index: Integer)
      : PCefStringUserFree; stdcall;

    // Returns the value of the attribute with the specified qualified name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_attribute_byqname: function(self: PCefXmlReader;
      const qualifiedName: PCefString): PCefStringUserFree; stdcall;

    // Returns the value of the attribute with the specified local name and
    // namespace URI.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_attribute_bylname: function(self: PCefXmlReader;
      const localName, namespaceURI: PCefString): PCefStringUserFree; stdcall;

    // Returns an XML representation of the current node's children.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_inner_xml: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns an XML representation of the current node including its children.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_outer_xml: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the line number for the current node.
    get_line_number: function(self: PCefXmlReader): Integer; stdcall;


    // Attribute nodes are not traversed by default. The below functions can be
    // used to move the cursor to an attribute node. move_to_carrying_element()
    // can be called afterwards to return the cursor to the carrying element. The
    // depth of an attribute node will be 1 + the depth of the carrying element.

    // Moves the cursor to the attribute at the specified 0-based index. Returns
    // true (1) if the cursor position was set successfully.
    move_to_attribute_byindex: function(self: PCefXmlReader; index: Integer)
      : Integer; stdcall;

    // Moves the cursor to the attribute with the specified qualified name.
    // Returns true (1) if the cursor position was set successfully.
    move_to_attribute_byqname: function(self: PCefXmlReader;
      const qualifiedName: PCefString): Integer; stdcall;

    // Moves the cursor to the attribute with the specified local name and
    // namespace URI. Returns true (1) if the cursor position was set
    // successfully.
    move_to_attribute_bylname: function(self: PCefXmlReader;
      const localName, namespaceURI: PCefString): Integer; stdcall;

    // Moves the cursor to the first attribute in the current element. Returns
    // true (1) if the cursor position was set successfully.
    move_to_first_attribute: function(self: PCefXmlReader): Integer; stdcall;

    // Moves the cursor to the next attribute in the current element. Returns true
    // (1) if the cursor position was set successfully.
    move_to_next_attribute: function(self: PCefXmlReader): Integer; stdcall;

    // Moves the cursor back to the carrying element. Returns true (1) if the
    // cursor position was set successfully.
    move_to_carrying_element: function(self: PCefXmlReader): Integer; stdcall;
  end;

  // Structure that supports the reading of zip archives via the zlib unzip API.
  // The functions of this structure should only be called on the thread that
  // creates the object.
  TCefZipReader = record
    // Base structure.
    base: TCefBase;

    // Moves the cursor to the first file in the archive. Returns true (1) if the
    // cursor position was set successfully.
    move_to_first_file: function(self: PCefZipReader): Integer; stdcall;

    // Moves the cursor to the next file in the archive. Returns true (1) if the
    // cursor position was set successfully.
    move_to_next_file: function(self: PCefZipReader): Integer; stdcall;

    // Moves the cursor to the specified file in the archive. If |caseSensitive|
    // is true (1) then the search will be case sensitive. Returns true (1) if the
    // cursor position was set successfully.
    move_to_file: function(self: PCefZipReader; const fileName: PCefString;
      caseSensitive: Integer): Integer; stdcall;

    // Closes the archive. This should be called directly to ensure that cleanup
    // occurs on the correct thread.
    close: function(self: PCefZipReader): Integer; stdcall;


    // The below functions act on the file at the current cursor position.

    // Returns the name of the file.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_file_name: function(self: PCefZipReader): PCefStringUserFree; stdcall;

    // Returns the uncompressed size of the file.
    get_file_size: function(self: PCefZipReader): Int64; stdcall;

    // Returns the last modified timestamp for the file.
    get_file_last_modified: function(self: PCefZipReader): TCefTime; stdcall;

    // Opens the file for reading of uncompressed data. A read password may
    // optionally be specified.
    open_file: function(self: PCefZipReader; const password: PCefString)
      : Integer; stdcall;

    // Closes the file.
    close_file: function(self: PCefZipReader): Integer; stdcall;

    // Read uncompressed file contents into the specified buffer. Returns < 0 if
    // an error occurred, 0 if at the end of file, or the number of bytes read.
    read_file: function(self: PCefZipReader; buffer: Pointer;
      bufferSize: NativeUInt): Integer; stdcall;

    // Returns the current offset in the uncompressed file contents.
    tell: function(self: PCefZipReader): Int64; stdcall;

    // Returns true (1) if at end of the file contents.
    eof: function(self: PCefZipReader): Integer; stdcall;
  end;

  // Structure to implement for visiting the DOM. The functions of this structure
  // will be called on the render process main thread.
  TCefDomVisitor = record
    // Base structure.
    base: TCefBase;

    // Method executed for visiting the DOM. The document object passed to this
    // function represents a snapshot of the DOM at the time this function is
    // executed. DOM objects are only valid for the scope of this function. Do not
    // keep references to or attempt to access any DOM objects outside the scope
    // of this function.
    visit: procedure(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
  end;

  // Structure used to represent a DOM document. The functions of this structure
  // should only be called on the render process main thread thread.
  TCefDomDocument = record
    // Base structure.
    base: TCefBase;

    // Returns the document type.
    get_type: function(self: PCefDomDocument): TCefDomDocumentType; stdcall;

    // Returns the root document node.
    get_document: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the BODY node of an HTML document.
    get_body: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the HEAD node of an HTML document.
    get_head: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the title of an HTML document.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_title: function(self: PCefDomDocument): PCefStringUserFree; stdcall;

    // Returns the document element with the specified ID value.
    get_element_by_id: function(self: PCefDomDocument; const id: PCefString)
      : PCefDomNode; stdcall;

    // Returns the node that currently has keyboard focus.
    get_focused_node: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns true (1) if a portion of the document is selected.
    has_selection: function(self: PCefDomDocument): Integer; stdcall;

    // Returns the selection offset within the start node.
    get_selection_start_offset: function(self: PCefDomDocument)
      : Integer; stdcall;

    // Returns the selection offset within the end node.
    get_selection_end_offset: function(self: PCefDomDocument): Integer; stdcall;

    // Returns the contents of this selection as markup.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_selection_as_markup: function(self: PCefDomDocument)
      : PCefStringUserFree; stdcall;

    // Returns the contents of this selection as text.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_selection_as_text: function(self: PCefDomDocument)
      : PCefStringUserFree; stdcall;

    // Returns the base URL for the document.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_base_url: function(self: PCefDomDocument): PCefStringUserFree; stdcall;

    // Returns a complete URL based on the document base URL and the specified
    // partial URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_complete_url: function(self: PCefDomDocument;
      const partialURL: PCefString): PCefStringUserFree; stdcall;
  end;

  // Structure used to represent a DOM node. The functions of this structure
  // should only be called on the render process main thread.
  TCefDomNode = record
    // Base structure.
    base: TCefBase;

    // Returns the type for this node.
    get_type: function(self: PCefDomNode): TCefDomNodeType; stdcall;

    // Returns true (1) if this is a text node.
    is_text: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this is an element node.
    is_element: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this is an editable node.
    is_editable: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this is a form control element node.
    is_form_control_element: function(self: PCefDomNode): Integer; stdcall;

    // Returns the type of this form control element node.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_form_control_element_type: function(self: PCefDomNode)
      : PCefStringUserFree; stdcall;

    // Returns true (1) if this object is pointing to the same handle as |that|
    // object.
    is_same: function(self, that: PCefDomNode): Integer; stdcall;

    // Returns the name of this node.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_name: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Returns the value of this node.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_value: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Set the value of this node. Returns true (1) on success.
    set_value: function(self: PCefDomNode; const value: PCefString)
      : Integer; stdcall;

    // Returns the contents of this node as markup.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_as_markup: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Returns the document associated with this node.
    get_document: function(self: PCefDomNode): PCefDomDocument; stdcall;

    // Returns the parent node.
    get_parent: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns the previous sibling node.
    get_previous_sibling: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns the next sibling node.
    get_next_sibling: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns true (1) if this node has child nodes.
    has_children: function(self: PCefDomNode): Integer; stdcall;

    // Return the first child node.
    get_first_child: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns the last child node.
    get_last_child: function(self: PCefDomNode): PCefDomNode; stdcall;

    // The following functions are valid only for element nodes.

    // Returns the tag name of this element.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_element_tag_name: function(self: PCefDomNode)
      : PCefStringUserFree; stdcall;

    // Returns true (1) if this element has attributes.
    has_element_attributes: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this element has an attribute named |attrName|.
    has_element_attribute: function(self: PCefDomNode;
      const attrName: PCefString): Integer; stdcall;

    // Returns the element attribute named |attrName|.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_element_attribute: function(self: PCefDomNode;
      const attrName: PCefString): PCefStringUserFree; stdcall;

    // Returns a map of all element attributes.
    get_element_attributes: procedure(self: PCefDomNode;
      attrMap: TCefStringMap); stdcall;

    // Set the value for the element attribute named |attrName|. Returns true (1)
    // on success.
    set_element_attribute: function(self: PCefDomNode;
      const attrName, value: PCefString): Integer; stdcall;

    // Returns the inner text of the element.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_element_inner_text: function(self: PCefDomNode)
      : PCefStringUserFree; stdcall;
  end;

  // Structure to implement for visiting cookie values. The functions of this
  // structure will always be called on the IO thread.
  TCefCookieVisitor = record
    // Base structure.
    base: TCefBase;

    // Method that will be called once for each cookie. |count| is the 0-based
    // index for the current cookie. |total| is the total number of cookies. Set
    // |deleteCookie| to true (1) to delete the cookie currently being visited.
    // Return false (0) to stop visiting cookies. This function may never be
    // called if no cookies are found.

    visit: function(self: PCefCookieVisitor; const cookie: PCefCookie;
      count, total: Integer; deleteCookie: PInteger): Integer; stdcall;
  end;

  // Structure to implement to be notified of asynchronous completion via
  // cef_cookie_manager_t::set_cookie().
  TCefSetCookieCallback = record
    // Base structure.
    base: TCefBase;

    // Method that will be called upon completion. |success| will be true (1) if
    // the cookie was set successfully.
    on_complete: procedure(self: PCefSetCookieCallback; success: Integer); stdcall;
  end;

  // Structure to implement to be notified of asynchronous completion via
  // cef_cookie_manager_t::delete_cookies().
  TCefDeleteCookiesCallback = record
    // Base structure.
    base: TCefBase;

    // Method that will be called upon completion. |num_deleted| will be the
    // number of cookies that were deleted or -1 if unknown.

    on_complete: procedure(self: PCefDeleteCookiesCallback; num_deleted: Integer); stdcall;
  end;

  // Structure used for managing cookies. The functions of this structure may be
  // called on any thread unless otherwise indicated.
  TCefCookieManager = record
    // Base structure.
    base: TCefBase;

    // Set the schemes supported by this manager. The default schemes ("http",
    // "https", "ws" and "wss") will always be supported. If |callback| is non-
    // NULL it will be executed asnychronously on the IO thread after the change
    // has been applied. Must be called before any cookies are accessed.
    set_supported_schemes: procedure(self: PCefCookieManager;
      schemes: TCefStringList; callback: PCefCompletionCallback); stdcall;

    // Visit all cookies on the IO thread. The returned cookies are ordered by
    // longest path, then by earliest creation date. Returns false (0) if cookies
    // cannot be accessed.
    visit_all_cookies: function(self: PCefCookieManager; visitor: PCefCookieVisitor): Integer; stdcall;

    // Visit a subset of cookies on the IO thread. The results are filtered by the
    // given url scheme, host, domain and path. If |includeHttpOnly| is true (1)
    // HTTP-only cookies will also be included in the results. The returned
    // cookies are ordered by longest path, then by earliest creation date.
    // Returns false (0) if cookies cannot be accessed.
    visit_url_cookies: function(self: PCefCookieManager; const url: PCefString;
      includeHttpOnly: Integer; visitor: PCefCookieVisitor): Integer; stdcall;

    // Sets a cookie given a valid URL and explicit user-provided cookie
    // attributes. This function expects each attribute to be well-formed. It will
    // check for disallowed characters (e.g. the ';' character is disallowed
    // within the cookie value attribute) and fail without setting the cookie if
    // such characters are found. If |callback| is non-NULL it will be executed
    // asnychronously on the IO thread after the cookie has been set. Returns
    // false (0) if an invalid URL is specified or if cookies cannot be accessed.
    set_cookie: function(self: PCefCookieManager; const url: PCefString;
      const cookie: PCefCookie; callback: PCefSetCookieCallback): Integer; stdcall;

    // Delete all cookies that match the specified parameters. If both |url| and
    // |cookie_name| values are specified all host and domain cookies matching
    // both will be deleted. If only |url| is specified all host cookies (but not
    // domain cookies) irrespective of path will be deleted. If |url| is NULL all
    // cookies for all hosts and domains will be deleted. If |callback| is non-
    // NULL it will be executed asnychronously on the IO thread after the cookies
    // have been deleted. Returns false (0) if a non-NULL invalid URL is specified
    // or if cookies cannot be accessed. Cookies can alternately be deleted using
    // the Visit*Cookies() functions.
    delete_cookies: function(self: PCefCookieManager; const url, cookie_name: PCefString;
      callback: PCefDeleteCookiesCallback): Integer; stdcall;

    // Sets the directory path that will be used for storing cookie data. If
    // |path| is NULL data will be stored in memory only. Otherwise, data will be
    // stored at the specified |path|. To persist session cookies (cookies without
    // an expiry date or validity interval) set |persist_session_cookies| to true
    // (1). Session cookies are generally intended to be transient and most Web
    // browsers do not persist them. If |callback| is non-NULL it will be executed
    // asnychronously on the IO thread after the manager's storage has been
    // initialized. Returns false (0) if cookies cannot be accessed.
    set_storage_path: function(self: PCefCookieManager;
      const path: PCefString; persist_session_cookies: Integer;
      callback: PCefCompletionCallback): Integer; stdcall;

    // Flush the backing store (if any) to disk and execute the specified
    // |callback| on the IO thread when done. Returns false (0) if cookies cannot
    // be accessed.
    flush_store: function(self: PCefCookieManager; handler: PCefCompletionCallback): Integer; stdcall;
  end;


  // Information about a specific web plugin.

  TCefWebPluginInfo = record
    // Base structure.
    base: TCefBase;

    // Returns the plugin name (i.e. Flash).
    get_name: function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;

    // Returns the plugin file path (DLL/bundle/library).
    get_path: function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;

    // Returns the version of the plugin (may be OS-specific).
    get_version: function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;

    // Returns a description of the plugin from the version information.
    get_description: function(self: PCefWebPluginInfo)
      : PCefStringUserFree; stdcall;
  end;

  // Structure to implement for visiting web plugin information. The functions of
  // this structure will be called on the browser process UI thread.
  TCefWebPluginInfoVisitor = record
    // Base structure.
    base: TCefBase;

    // Method that will be called once for each plugin. |count| is the 0-based
    // index for the current plugin. |total| is the total number of plugins.
    // Return false (0) to stop visiting plugins. This function may never be
    // called if no plugins are found.
    visit: function(self: PCefWebPluginInfoVisitor; info: PCefWebPluginInfo;
      count, total: Integer): Integer; stdcall;
  end;

  // Structure to implement for receiving unstable plugin information. The
  // functions of this structure will be called on the browser process IO thread.
  TCefWebPluginUnstableCallback = record
    // Base structure.
    base: TCefBase;

    // Method that will be called for the requested plugin. |unstable| will be
    // true (1) if the plugin has reached the crash count threshold of 3 times in
    // 120 seconds.
    is_unstable: procedure(self: PCefWebPluginUnstableCallback;
      const path: PCefString; unstable: Integer); stdcall;
  end;

  // Structure used to make a URL request. URL requests are not associated with a
  // browser instance so no cef_client_t callbacks will be executed. URL requests
  // can be created on any valid CEF thread in either the browser or render
  // process. Once created the functions of the URL request object must be
  // accessed on the same thread that created it.
  TCefUrlRequest = record
    // Base structure.
    base: TCefBase;

    // Returns the request object used to create this URL request. The returned
    // object is read-only and should not be modified.
    get_request: function(self: PCefUrlRequest): PCefRequest; stdcall;

    // Returns the client.
    get_client: function(self: PCefUrlRequest): PCefUrlRequestClient; stdcall;

    // Returns the request status.
    get_request_status: function(self: PCefUrlRequest)
      : TCefUrlRequestStatus; stdcall;

    // Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
    // otherwise.
    get_request_error: function(self: PCefUrlRequest): TCefErrorcode; stdcall;

    // Returns the response, or NULL if no response information is available.
    // Response information will only be available after the upload has completed.
    // The returned object is read-only and should not be modified.
    get_response: function(self: PCefUrlRequest): PCefResponse; stdcall;

    // Cancel the request.
    cancel: procedure(self: PCefUrlRequest); stdcall;
  end;

  // Structure that should be implemented by the cef_urlrequest_t client. The
  // functions of this structure will be called on the same thread that created
  // the request unless otherwise documented.
  TCefUrlRequestClient = record
    // Base structure.
    base: TCefBase;

    // Notifies the client that the request has completed. Use the
    // cef_urlrequest_t::GetRequestStatus function to determine if the request was
    // successful or not.
    on_request_complete: procedure(self: PCefUrlRequestClient;
      request: PCefUrlRequest); stdcall;

    // Notifies the client of upload progress. |current| denotes the number of
    // bytes sent so far and |total| is the total size of uploading data (or -1 if
    // chunked upload is enabled). This function will only be called if the
    // UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
    on_upload_progress: procedure(self: PCefUrlRequestClient;
      request: PCefUrlRequest; current, total: UInt64); stdcall;

    // Notifies the client of download progress. |current| denotes the number of
    // bytes received up to the call and |total| is the expected total size of the
    // response (or -1 if not determined).
    on_download_progress: procedure(self: PCefUrlRequestClient;
      request: PCefUrlRequest; current, total: UInt64); stdcall;

    // Called when some part of the response is read. |data| contains the current
    // bytes received since the last call. This function will not be called if the
    // UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
    on_download_data: procedure(self: PCefUrlRequestClient;
      request: PCefUrlRequest; const data: Pointer;
      data_length: NativeUInt); stdcall;

    // Called on the IO thread when the browser needs credentials from the user.
    // |isProxy| indicates whether the host is a proxy server. |host| contains the
    // hostname and |port| contains the port number. Return true (1) to continue
    // the request and call cef_auth_callback_t::cont() when the authentication
    // information is available. Return false (0) to cancel the request. This
    // function will only be called for requests initiated from the browser
    // process.
    get_auth_credentials: function(self: PCefUrlRequestClient; isProxy: Integer;
      const host: PCefString; port: Integer; const realm, scheme: PCefString;
      callback: PCefAuthCallback): Integer; stdcall;
  end;

  // Callback structure for asynchronous continuation of file dialog requests.
  TCefFileDialogCallback = record
    // Base structure.
    base: TCefBase;

    // Continue the file selection. |selected_accept_filter| should be the 0-based
    // index of the value selected from the accept filters array passed to
    // cef_dialog_handler_t::OnFileDialog. |file_paths| should be a single value
    // or a list of values depending on the dialog mode. An NULL |file_paths|
    cont: procedure(self: PCefFileDialogCallback; selected_accept_filter: Integer;
     file_paths: TCefStringList); stdcall;

    // Cancel the file selection.
    cancel: procedure(self: PCefFileDialogCallback); stdcall;
  end;

  // Implement this structure to handle dialog events. The functions of this
  // structure will be called on the browser process UI thread.
  TCefDialogHandler = record
    // Base structure.
    base: TCefBase;

    // Called to run a file chooser dialog. |mode| represents the type of dialog
    // to display. |title| to the title to be used for the dialog and may be NULL
    // to show the default title ("Open" or "Save" depending on the mode).
    // |default_file_path| is the path with optional directory and/or file name
    // component that should be initially selected in the dialog. |accept_filters|
    // are used to restrict the selectable file types and may any combination of
    // (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
    // individual file extensions (e.g. ".txt" or ".png"), or (c) combined
    // description and file extension delimited using "|" and ";" (e.g. "Image
    // Types|.png;.gif;.jpg"). |selected_accept_filter| is the 0-based index of
    // the filter that should be selected by default. To display a custom dialog
    // return true (1) and execute |callback| either inline or at a later time. To
    // display the default dialog return false (0).
    on_file_dialog: function(self: PCefDialogHandler; browser: PCefBrowser;
      mode: TCefFileDialogMode; const title, default_file_path: PCefString;
      accept_filters: TCefStringList; selected_accept_filter: Integer;
      callback: PCefFileDialogCallback): Integer; stdcall;
  end;

  // Implement this structure to handle events when window rendering is disabled.
  // The functions of this structure will be called on the UI thread.
  TCefRenderHandler = record
    // Base structure.
    base: TCefBase;

    // Called to retrieve the root window rectangle in screen coordinates. Return
    // true (1) if the rectangle was provided.
    get_root_screen_rect: function(self: PCefRenderHandler;
      browser: PCefBrowser; rect: PCefRect): Integer; stdcall;

    // Called to retrieve the view rectangle which is relative to screen
    // coordinates. Return true (1) if the rectangle was provided.
    get_view_rect: function(self: PCefRenderHandler; browser: PCefBrowser;
      rect: PCefRect): Integer; stdcall;

    // Called to retrieve the translation from view coordinates to actual screen
    // coordinates. Return true (1) if the screen coordinates were provided.
    get_screen_point: function(self: PCefRenderHandler; browser: PCefBrowser;
      viewX, viewY: Integer; screenX, screenY: PInteger): Integer; stdcall;

    // Called to allow the client to fill in the CefScreenInfo object with
    // appropriate values. Return true (1) if the |screen_info| structure has been
    // modified.
    //
    // If the screen info rectangle is left NULL the rectangle from GetViewRect
    // will be used. If the rectangle is still NULL or invalid popups may not be
    // drawn correctly.
    get_screen_info: function(self: PCefRenderHandler; browser: PCefBrowser;
      screen_info: PCefScreenInfo): Integer; stdcall;

    // Called when the browser wants to show or hide the popup widget. The popup
    // should be shown if |show| is true (1) and hidden if |show| is false (0).
    on_popup_show: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; show: Integer); stdcall;

    // Called when the browser wants to move or resize the popup widget. |rect|
    // contains the new location and size in view coordinates.
    on_popup_size: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; const rect: PCefRect); stdcall;

    // Called when an element should be painted. Pixel values passed to this
    // function are scaled relative to view coordinates based on the value of
    // CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
    // indicates whether the element is the view or the popup widget. |buffer|
    // contains the pixel data for the whole image. |dirtyRects| contains the set
    // of rectangles in pixel coordinates that need to be repainted. |buffer| will
    // be |width|*|height|*4 bytes in size and represents a BGRA image with an
    // upper-left origin.
    on_paint: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser;
      kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
      const dirtyRects: PCefRectArray; const buffer: Pointer;
      width, height: Integer); stdcall;

    // Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
    // |custom_cursor_info| will be populated with the custom cursor information.
    on_cursor_change: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; cursor: TCefCursorHandle; type_: TCefCursorType;
      const custom_cursor_info: PCefCursorInfo); stdcall;

    // Called when the user starts dragging content in the web view. Contextual
    // information about the dragged content is supplied by |drag_data|. (|x|,
    // |y|) is the drag start location in screen coordinates. OS APIs that run a
    // system message loop may be used within the StartDragging call.
    //
    // Return false (0) to abort the drag operation. Don't call any of
    // cef_browser_host_t::DragSource*Ended* functions after returning false (0).
    //
    // Return true (1) to handle the drag operation. Call
    // cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
    // synchronously or asynchronously to inform the web view that the drag
    // operation has ended.
    start_dragging: function(self: PCefRenderProcessHandler;
      browser: PCefBrowser; drag_data: PCefDragData;
      allowed_ops: TCefDragOperations; x, y: Integer): Integer; stdcall;

    // Called when the web view wants to update the mouse cursor during a drag &
    // drop operation. |operation| describes the allowed operation (none, move,
    // copy, link).
    update_drag_cursor: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; operation: TCefDragOperation); stdcall;

    on_scroll_offset_changed: procedure(self: PCefRenderProcessHandler;
      browser: PCefBrowser; x, y: Double); stdcall;
  end;

  // Implement this structure to receive geolocation updates. The functions of
  // this structure will be called on the browser process UI thread.
  TCefGetGeolocationCallback = record
    // Base structure.
    base: TCefBase;

    // Called with the 'best available' location information or, if the location
    // update failed, with error information.
    on_location_update: procedure(self: PCefGetGeolocationCallback;
      const position: PCefGeoposition); stdcall;
  end;

  // Implement this structure to receive notification when tracing has completed.
  // The functions of this structure will be called on the browser process UI
  // thread.
  TCefEndTracingCallback = record
    // Base structure.
    base: TCefBase;

    // Called after all processes have sent their trace data. |tracing_file| is
    // the path at which tracing data was written. The client is responsible for
    // deleting |tracing_file|.
    on_end_tracing_complete: procedure(self: PCefEndTracingCallback;
      const tracing_file: PCefString); stdcall;
  end;

  TCefDragData = record
    // Base structure.
    base: TCefBase;

    // Returns a copy of the current object.
    clone: function(self: PCefDragData): PCefDragData; stdcall;

    // Returns true (1) if this object is read-only.
    is_read_only: function(self: PCefDragData): Integer; stdcall;

    // Returns true (1) if the drag data is a link.
    is_link: function(self: PCefDragData): Integer; stdcall;

    // Returns true (1) if the drag data is a text or html fragment.
    is_fragment: function(self: PCefDragData): Integer; stdcall;

    // Returns true (1) if the drag data is a file.
    is_file: function(self: PCefDragData): Integer; stdcall;

    // Return the link URL that is being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_url: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the title associated with the link being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_title: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the metadata, if any, associated with the link being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_metadata: function(self: PCefDragData)
      : PCefStringUserFree; stdcall;

    // Return the plain text fragment that is being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_fragment_text: function(self: PCefDragData)
      : PCefStringUserFree; stdcall;

    // Return the text/html fragment that is being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_fragment_html: function(self: PCefDragData)
      : PCefStringUserFree; stdcall;

    // Return the base URL that the fragment came from. This value is used for
    // resolving relative URLs and may be NULL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_fragment_base_url: function(self: PCefDragData)
      : PCefStringUserFree; stdcall;

    // Return the name of the file being dragged out of the browser window.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_file_name: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Write the contents of the file being dragged out of the web view into
    // |writer|. Returns the number of bytes sent to |writer|. If |writer| is NULL
    // this function will return the size of the file contents in bytes. Call
    // get_file_name() to get a suggested name for the file.
    get_file_contents: function(self: PCefDragData; writer: PCefStreamWriter)
      : NativeUInt; stdcall;

    // Retrieve the list of file names that are being dragged into the browser
    // window.
    get_file_names: function(self: PCefDragData; names: TCefStringList)
      : Integer; stdcall;

    // Set the link URL that is being dragged.
    set_link_url: procedure(self: PCefDragData; const url: PCefString); stdcall;

    // Set the title associated with the link being dragged.
    set_link_title: procedure(self: PCefDragData;
      const title: PCefString); stdcall;

    // Set the metadata associated with the link being dragged.
    set_link_metadata: procedure(self: PCefDragData;
      const data: PCefString); stdcall;

    // Set the plain text fragment that is being dragged.
    set_fragment_text: procedure(self: PCefDragData;
      const text: PCefString); stdcall;

    // Set the text/html fragment that is being dragged.
    set_fragment_html: procedure(self: PCefDragData;
      const html: PCefString); stdcall;

    // Set the base URL that the fragment came from.
    set_fragment_base_url: procedure(self: PCefDragData;
      const base_url: PCefString); stdcall;

    // Reset the file contents. You should do this before calling
    // cef_browser_host_t::DragTargetDragEnter as the web view does not allow us
    // to drag in this kind of data.
    reset_file_contents: procedure(self: PCefDragData); stdcall;

    // Add a file that is being dragged into the webview.
    add_file: procedure(self: PCefDragData;
      const path, display_name: PCefString); stdcall;
  end;

  // Implement this structure to handle events related to dragging. The functions
  // of this structure will be called on the UI thread.
  TCefDragHandler = record
    // Base structure.
    base: TCefBase;

    // Called when an external drag event enters the browser window. |dragData|
    // contains the drag event data and |mask| represents the type of drag
    // operation. Return false (0) for default drag handling behavior or true (1)
    // to cancel the drag event.
    on_drag_enter: function(self: PCefDragHandler; browser: PCefBrowser;
      dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
{$ifdef Win32}
    // Called whenever draggable regions for the browser window change. These can
    // be specified using the '-webkit-app-region: drag/no-drag' CSS-property. If
    // draggable regions are never defined in a document this function will also
    // never be called. If the last draggable region is removed from a document
    // this function will be called with an NULL vector.
    on_draggable_regions_changed: procedure(self: PCefDragHandler; browser: PCefBrowser;
      regionsCount: NativeUInt; regions: PCefDraggableRegionArray); stdcall;
{$endif}
  end;

  // Implement this structure to provide handler implementations. The handler
  // instance will not be released until all objects related to the context have
  // been destroyed.
  TCefRequestContextHandler = record
    // Base structure.
    base: TCefBase;

    // Called on the browser process IO thread to retrieve the cookie manager. If
    // this function returns NULL the default cookie manager retrievable via
    // cef_request_tContext::get_default_cookie_manager() will be used.
    get_cookie_manager: function(self: PCefRequestContextHandler): PCefCookieManager; stdcall;

    // Called on multiple browser process threads before a plugin instance is
    // loaded. |mime_type| is the mime type of the plugin that will be loaded.
    // |plugin_url| is the content URL that the plugin will load and may be NULL.
    // |top_origin_url| is the URL for the top-level frame that contains the
    // plugin when loading a specific plugin instance or NULL when building the
    // initial list of enabled plugins for 'navigator.plugins' JavaScript state.
    // |plugin_info| includes additional information about the plugin that will be
    // loaded. |plugin_policy| is the recommended policy. Modify |plugin_policy|
    // and return true (1) to change the policy. Return false (0) to use the
    // recommended policy. The default plugin policy can be set at runtime using
    // the `--plugin-policy=[allow|detect|block]` command-line flag. Decisions to
    // mark a plugin as disabled by setting |plugin_policy| to
    // PLUGIN_POLICY_DISABLED may be cached when |top_origin_url| is NULL. To
    // purge the plugin list cache and potentially trigger new calls to this
    // function call cef_request_tContext::PurgePluginListCache.
    on_before_plugin_load: function(self: PCefRequestContextHandler;
        const mime_type, plugin_url, top_origin_url: PCefString;
        plugin_info: PCefWebPluginInfo; plugin_policy: PCefPluginPolicy): Integer; stdcall;
  end;

  // Callback structure for cef_request_tContext::ResolveHost.
  PCefResolveCallback = ^TCefResolveCallback;
  TCefResolveCallback = record
    // Base structure.
    base: TCefBase;

    // Called after the ResolveHost request has completed. |result| will be the
    // result code. |resolved_ips| will be the list of resolved IP addresses or
    // NULL if the resolution failed.
    on_resolve_completed: procedure(self: PCefResolveCallback;
      result: TCefErrorCode; resolved_ips: TCefStringList); stdcall;
  end;

  // A request context provides request handling for a set of related browser or
  // URL request objects. A request context can be specified when creating a new
  // browser via the cef_browser_host_t static factory functions or when creating
  // a new URL request via the cef_urlrequest_t static factory functions. Browser
  // objects with different request contexts will never be hosted in the same
  // render process. Browser objects with the same request context may or may not
  // be hosted in the same render process depending on the process model. Browser
  // objects created indirectly via the JavaScript window.open function or
  // targeted links will share the same render process and the same request
  // context as the source browser. When running in single-process mode there is
  // only a single render process (the main process) and so all browsers created
  // in single-process mode will share the same request context. This will be the
  // first request context passed into a cef_browser_host_t static factory
  // function and all other request context objects will be ignored.
  TCefRequestContext = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is pointing to the same context as |that|
    // object.
    is_same: function(self, other: PCefRequestContext): Integer; stdcall;

    // Returns true (1) if this object is sharing the same storage as |that|
    // object.
    is_sharing_with: function(self, other: PCefRequestContext): Integer; stdcall;

    // Returns true (1) if this object is the global context. The global context
    // is used by default when creating a browser or URL request with a NULL
    // context argument.
    is_global: function(self: PCefRequestContext): Integer; stdcall;

    // Returns the handler for this context if any.
    get_handler: function(self: PCefRequestContext): PCefRequestContextHandler; stdcall;

    // Returns the cache path for this object. If NULL an "incognito mode" in-
    // memory cache is being used.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_cache_path: function(self: PCefRequestContext): PCefStringUserFree; stdcall;

    // Returns the default cookie manager for this object. This will be the global
    // cookie manager if this object is the global request context. Otherwise,
    // this will be the default cookie manager used when this request context does
    // not receive a value via cef_request_tContextHandler::get_cookie_manager().
    // If |callback| is non-NULL it will be executed asnychronously on the IO
    // thread after the manager's storage has been initialized.
    get_default_cookie_manager: function(self: PCefRequestContext;
      callback: PCefCompletionCallback): PCefCookieManager; stdcall;

    // Register a scheme handler factory for the specified |scheme_name| and
    // optional |domain_name|. An NULL |domain_name| value for a standard scheme
    // will cause the factory to match all domain names. The |domain_name| value
    // will be ignored for non-standard schemes. If |scheme_name| is a built-in
    // scheme and no handler is returned by |factory| then the built-in scheme
    // handler factory will be called. If |scheme_name| is a custom scheme then
    // you must also implement the cef_app_t::on_register_custom_schemes()
    // function in all processes. This function may be called multiple times to
    // change or remove the factory that matches the specified |scheme_name| and
    // optional |domain_name|. Returns false (0) if an error occurs. This function
    // may be called on any thread in the browser process.
    register_scheme_handler_factory: function(self: PCefRequestContext;
        const scheme_name, domain_name: PCefString;
        factory: PCefSchemeHandlerFactory): Integer; stdcall;

    // Clear all registered scheme handler factories. Returns false (0) on error.
    // This function may be called on any thread in the browser process.
    clear_scheme_handler_factories: function(self: PCefRequestContext): Integer; stdcall;

    // Tells all renderer processes associated with this context to throw away
    // their plugin list cache. If |reload_pages| is true (1) they will also
    // reload all pages with plugins.
    // cef_request_tContextHandler::OnBeforePluginLoad may be called to rebuild
    // the plugin list cache.
    purge_plugin_list_cache: procedure(self: PCefRequestContext; reload_pages: Integer); stdcall;

    // Returns true (1) if a preference with the specified |name| exists. This
    // function must be called on the browser process UI thread.
    has_preference: function(self: PCefRequestContext; const name: PCefString): Integer; stdcall;

    // Returns the value for the preference with the specified |name|. Returns
    // NULL if the preference does not exist. The returned object contains a copy
    // of the underlying preference value and modifications to the returned object
    // will not modify the underlying preference value. This function must be
    // called on the browser process UI thread.
    get_preference: function(self: PCefRequestContext; const name: PCefString): PCefValue; stdcall;

    // Returns all preferences as a dictionary. If |include_defaults| is true (1)
    // then preferences currently at their default value will be included. The
    // returned object contains a copy of the underlying preference values and
    // modifications to the returned object will not modify the underlying
    // preference values. This function must be called on the browser process UI
    // thread.
    get_all_preferences: function(self: PCefRequestContext; include_defaults: Integer): PCefDictionaryValue; stdcall;

    // Returns true (1) if the preference with the specified |name| can be
    // modified using SetPreference. As one example preferences set via the
    // command-line usually cannot be modified. This function must be called on
    // the browser process UI thread.
    can_set_preference: function(self: PCefRequestContext; const name: PCefString): Integer; stdcall;

    // Set the |value| associated with preference |name|. Returns true (1) if the
    // value is set successfully and false (0) otherwise. If |value| is NULL the
    // preference will be restored to its default value. If setting the preference
    // fails then |error| will be populated with a detailed description of the
    // problem. This function must be called on the browser process UI thread.
    set_preference: function(self: PCefRequestContext; const name: PCefString;
      value: PCefValue; error: PCefString): Integer; stdcall;

    // Clears all certificate exceptions that were added as part of handling
    // cef_request_tHandler::on_certificate_error(). If you call this it is
    // recommended that you also call close_all_connections() or you risk not
    // being prompted again for server certificates if you reconnect quickly. If
    // |callback| is non-NULL it will be executed on the UI thread after
    // completion.
    clear_certificate_exceptions: procedure(self: PCefRequestContext;
      callback: PCefCompletionCallback); stdcall;

    // Clears all active and idle connections that Chromium currently has. This is
    // only recommended if you have released all other CEF objects but don't yet
    // want to call cef_shutdown(). If |callback| is non-NULL it will be executed
    // on the UI thread after completion.
    close_all_connections: procedure(self: PCefRequestContext;
      callback: PCefCompletionCallback); stdcall;

    // Attempts to resolve |origin| to a list of associated IP addresses.
    // |callback| will be executed on the UI thread after completion.
    resolve_host: procedure(self: PCefRequestContext; const origin: PCefString;
      callback: PCefResolveCallback); stdcall;

    // Attempts to resolve |origin| to a list of associated IP addresses using
    // cached data. |resolved_ips| will be populated with the list of resolved IP
    // addresses or NULL if no cached data is available. Returns ERR_NONE on
    // success. This function must be called on the browser process IO thread.
    resolve_host_cached: function(self: PCefRequestContext; const origin: PCefString;
      resolved_ips: TCefStringList): TCefErrorCode; stdcall;
  end;

  // Structure representing print settings.
  TCefPrintSettings = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. Do not call any other functions
    // if this function returns false (0).
    is_valid: function(self: PCefPrintSettings): Integer; stdcall;

    // Returns true (1) if the values of this object are read-only. Some APIs may
    // expose read-only objects.
    is_read_only: function(self: PCefPrintSettings): Integer; stdcall;

    // Returns a writable copy of this object.
    copy: function(self: PCefPrintSettings): PCefPrintSettings; stdcall;

    // Set the page orientation.
    set_orientation: procedure(self: PCefPrintSettings;
      landscape: Integer); stdcall;

    // Returns true (1) if the orientation is landscape.
    is_landscape: function(self: PCefPrintSettings): Integer; stdcall;

    // Set the printer printable area in device units. Some platforms already
    // provide flipped area. Set |landscape_needs_flip| to false (0) on those
    // platforms to avoid double flipping.
    set_printer_printable_area: procedure(self: PCefPrintSettings;
      const physical_size_device_units: PCefSize;
      const printable_area_device_units: PCefRect;
      landscape_needs_flip: Integer); stdcall;

    // Set the device name.
    set_device_name: procedure(self: PCefPrintSettings;
      const name: PCefString); stdcall;

    // Get the device name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_device_name: function(self: PCefPrintSettings)
      : PCefStringUserFree; stdcall;

    // Set the DPI (dots per inch).
    set_dpi: procedure(self: PCefPrintSettings; dpi: Integer); stdcall;

    // Get the DPI (dots per inch).
    get_dpi: function(self: PCefPrintSettings): Integer; stdcall;

    // Set the page ranges.
    set_page_ranges: procedure(self: PCefPrintSettings; rangesCount: NativeUInt;
      ranges: PCefPageRange); stdcall;

    // Returns the number of page ranges that currently exist.
    get_page_ranges_count: function(self: PCefPrintSettings)
      : NativeUInt; stdcall;

    // Retrieve the page ranges.
    get_page_ranges: procedure(self: PCefPrintSettings;
      rangesCount: PNativeUInt; ranges: PCefPageRange); stdcall;

    // Set whether only the selection will be printed.
    set_selection_only: procedure(self: PCefPrintSettings;
      selection_only: Integer); stdcall;

    // Returns true (1) if only the selection will be printed.
    is_selection_only: function(self: PCefPrintSettings): Integer; stdcall;

    // Set whether pages will be collated.
    set_collate: procedure(self: PCefPrintSettings; collate: Integer); stdcall;

    // Returns true (1) if pages will be collated.
    will_collate: function(self: PCefPrintSettings): Integer; stdcall;

    // Set the color model.
    set_color_model: procedure(self: PCefPrintSettings;
      model: TCefColorModel); stdcall;

    // Get the color model.
    get_color_model: function(self: PCefPrintSettings): TCefColorModel; stdcall;

    // Set the number of copies.
    set_copies: procedure(self: PCefPrintSettings; copies: Integer); stdcall;

    // Get the number of copies.
    get_copies: function(self: PCefPrintSettings): Integer; stdcall;

    // Set the duplex mode.
    set_duplex_mode: procedure(self: PCefPrintSettings;
      mode: TCefDuplexMode); stdcall;

    // Get the duplex mode.
    get_duplex_mode: function(self: PCefPrintSettings): TCefDuplexMode; stdcall;
  end;

  // Callback structure for asynchronous continuation of print dialog requests.
  TCefPrintDialogCallback = record
    // Base structure.
    base: TCefBase;

    // Continue printing with the specified |settings|.
    cont: procedure(self: PCefPrintDialogCallback;
      settings: PCefPrintSettings); stdcall;

    // Cancel the printing.
    cancel: procedure(self: PCefPrintDialogCallback); stdcall;
  end;

  // Callback structure for asynchronous continuation of print job requests.
  TCefPrintJobCallback = record
    // Base structure.
    base: TCefBase;

    // Indicate completion of the print job.
    cont: procedure(self: PCefPrintJobCallback); stdcall;
  end;

  // Implement this structure to handle printing on Linux. The functions of this
  // structure will be called on the browser process UI thread.
  TCefPrintHandler = record
    // Base structure.
    base: TCefBase;

    // Called when printing has started for the specified |browser|. This function
    // will be called before the other OnPrint*() functions and irrespective of
    // how printing was initiated (e.g. cef_browser_host_t::print(), JavaScript
    // window.print() or PDF extension print button).
    on_print_start: procedure(self: PCefPrintHandler; browser: PCefBrowser); stdcall;

    // Synchronize |settings| with client state. If |get_defaults| is true (1)
    // then populate |settings| with the default print settings. Do not keep a
    // reference to |settings| outside of this callback.
    ///
    on_print_settings: procedure(self: PCefPrintHandler;
      settings: PCefPrintSettings; get_defaults: Integer); stdcall;

    ///
    // Show the print dialog. Execute |callback| once the dialog is dismissed.
    // Return true (1) if the dialog will be displayed or false (0) to cancel the
    // printing immediately.
    ///
    on_print_dialog: function(self: PCefPrintHandler; has_selection: Integer;
      callback: PCefPrintDialogCallback): Integer; stdcall;

    // Send the print job to the printer. Execute |callback| once the job is
    // completed. Return true (1) if the job will proceed or false (0) to cancel
    // the job immediately.
    on_print_job: function(self: PCefPrintHandler;
      const document_name, pdf_file_path: PCefString;
      callback: PCefPrintJobCallback): Integer; stdcall;

    // Reset client state related to printing.
    on_print_reset: procedure(self: PCefPrintHandler); stdcall;

    // Return the PDF paper size in device units. Used in combination with
    // cef_browser_host_t::print_to_pdf().
    get_pdf_paper_size: function(self: PCefPrintHandler;
      device_units_per_inch: Integer): TCefSize; stdcall;
  end;

  // Structure used to represent an entry in navigation history.
  TCefNavigationEntry = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if this object is valid. Do not call any other functions
    // if this function returns false (0).
    is_valid: function(self: PCefNavigationEntry): Integer; stdcall;

    // Returns the actual URL of the page. For some pages this may be data: URL or
    // similar. Use get_display_url() to return a display-friendly version.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_url: function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;

    // Returns a display-friendly version of the URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_display_url: function(self: PCefNavigationEntry)
      : PCefStringUserFree; stdcall;

    // Returns the original URL that was entered by the user before any redirects.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_original_url: function(self: PCefNavigationEntry)
      : PCefStringUserFree; stdcall;

    // Returns the title set by the page. This value may be NULL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_title: function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;

    // Returns the transition type which indicates what the user did to move to
    // this page from the previous page.
    get_transition_type: function(self: PCefNavigationEntry)
      : TCefTransitionType; stdcall;

    // Returns true (1) if this navigation includes post data.
    has_post_data: function(self: PCefNavigationEntry): Integer; stdcall;

    // Returns the time for the last known successful navigation completion. A
    // navigation may be completed more than once if the page is reloaded. May be
    // 0 if the navigation has not yet completed.
    get_completion_time: function(self: PCefNavigationEntry): TCefTime; stdcall;

    // Returns the HTTP status code for the last known successful navigation
    // response. May be 0 if the response has not yet been received or if the
    // navigation has not yet completed.
    get_http_status_code: function(self: PCefNavigationEntry): Integer; stdcall;
  end;

  // Structure representing the issuer or subject field of an X.509 certificate.
  TCefSslCertPrincipal = record
    // Base structure.
    base: TCefBase;

    // Returns a name that can be used to represent the issuer.  It tries in this
    // order: CN, O and OU and returns the first non-NULL one found.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_display_name: function(self: PCefSslCertPrincipal): PCefStringUserfree; stdcall;

    // Returns the common name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_common_name: function(self: PCefSslCertPrincipal): PCefStringUserfree; stdcall;

    // Returns the locality name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_locality_name: function(self: PCefSslCertPrincipal): PCefStringUserfree; stdcall;

    // Returns the state or province name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_state_or_province_name: function(self: PCefSslCertPrincipal): PCefStringUserfree; stdcall;

    // Returns the country name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_country_name: function(self: PCefSslCertPrincipal): PCefStringUserfree; stdcall;

    // Retrieve the list of street addresses.
    get_street_addresses: procedure(self: PCefSslCertPrincipal; addresses: TCefStringList); stdcall;

    // Retrieve the list of organization names.
    get_organization_names: procedure(self: PCefSslCertPrincipal; names: TCefStringList); stdcall;

    // Retrieve the list of organization unit names.
    get_organization_unit_names: procedure(self: PCefSslCertPrincipal; names: TCefStringList); stdcall;

    // Retrieve the list of domain components.
    get_domain_components: procedure(self: PCefSslCertPrincipal; components: TCefStringList); stdcall;
  end;

  // Structure representing SSL information.
  TCefSslInfo = record
    // Base structure.
    base: TCefBase;

    // Returns a bitmask containing any and all problems verifying the server
    // certificate.
    get_cert_status: function(self: PCefSslInfo): TCefCertStatus; stdcall;

    // Returns true (1) if the certificate status has any error, major or minor.
    is_cert_status_error: function(self: PCefSslInfo): Integer; stdcall;

    // Returns true (1) if the certificate status represents only minor errors
    // (e.g. failure to verify certificate revocation).
    is_cert_status_minor_error: function(self: PCefSslInfo): Integer; stdcall;

    // Returns the subject of the X.509 certificate. For HTTPS server certificates
    // this represents the web server.  The common name of the subject should
    // match the host name of the web server.
    get_subject: function(self: PCefSslInfo): PCefSslCertPrincipal; stdcall;

    // Returns the issuer of the X.509 certificate.
    get_issuer: function(self: PCefSslInfo): PCefSslCertPrincipal; stdcall;

    // Returns the DER encoded serial number for the X.509 certificate. The value
    // possibly includes a leading 00 byte.
    get_serial_number: function(self: PCefSslInfo): PCefBinaryValue; stdcall;

    // Returns the date before which the X.509 certificate is invalid.
    // CefTime.GetTimeT() will return 0 if no date was specified.
    get_valid_start: function(self: PCefSslInfo): TCefTime; stdcall;

    // Returns the date after which the X.509 certificate is invalid.
    // CefTime.GetTimeT() will return 0 if no date was specified.
    get_valid_expiry: function(self: PCefSslInfo): TCefTime; stdcall;

    // Returns the DER encoded data for the X.509 certificate.
    get_derencoded: function(self: PCefSslInfo): PCefBinaryValue; stdcall;

    // Returns the PEM encoded data for the X.509 certificate.
    get_pemencoded: function(self: PCefSslInfo): PCefBinaryValue; stdcall;

    // Returns the number of certificates in the issuer chain. If 0, the
    // certificate is self-signed.
    get_issuer_chain_size: function(self: PCefSslInfo): NativeUInt; stdcall;

    // Returns the DER encoded data for the certificate issuer chain. If we failed
    // to encode a certificate in the chain it is still present in the array but
    // is an NULL string.
    get_derencoded_issuer_chain: procedure(self: PCefSslInfo;
        chainCount: NativeUInt; chain: PPCefBinaryValue); stdcall;

    // Returns the PEM encoded data for the certificate issuer chain. If we failed
    // to encode a certificate in the chain it is still present in the array but
    // is an NULL string.
    get_pemencoded_issuer_chain: procedure(self: PCefSslInfo;
      out chainCount: NativeUInt; chain: PPCefBinaryValue); stdcall;
  end;

  // Structure used for retrieving resources from the resource bundle (*.pak)
  // files loaded by CEF during startup or via the cef_resource_bundle_tHandler
  // returned from cef_app_t::GetResourceBundleHandler. See CefSettings for
  // additional options related to resource bundle loading. The functions of this
  // structure may be called on any thread unless otherwise indicated.
  TCefResourceBundle = record
    // Base structure.
    base: TCefBase;

    // Returns the localized string for the specified |string_id| or an NULL
    // string if the value is not found. Include cef_pack_strings.h for a listing
    // of valid string ID values.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_localized_string: function(self: PCefResourceBundle; string_id: Integer): PCefStringUserFree; stdcall;

    // Retrieves the contents of the specified scale independent |resource_id|. If
    // the value is found then |data| and |data_size| will be populated and this
    // function will return true (1). If the value is not found then this function
    // will return false (0). The returned |data| pointer will remain resident in
    // memory and should not be freed. Include cef_pack_resources.h for a listing
    // of valid resource ID values.
    get_data_resource: function(self: PCefResourceBundle; resource_id: Integer;
      out data: Pointer; out data_size: NativeUInt): Integer; stdcall;

    // Retrieves the contents of the specified |resource_id| nearest the scale
    // factor |scale_factor|. Use a |scale_factor| value of SCALE_FACTOR_NONE for
    // scale independent resources or call GetDataResource instead. If the value
    // is found then |data| and |data_size| will be populated and this function
    // will return true (1). If the value is not found then this function will
    // return false (0). The returned |data| pointer will remain resident in
    // memory and should not be freed. Include cef_pack_resources.h for a listing
    // of valid resource ID values.
     get_data_resource_for_scale: function(self: PCefResourceBundle; resource_id: Integer;
       scale_factor: TCefScaleFactor; out data: Pointer; out data_size: NativeUInt): Integer; stdcall;
  end;

  // Implement this structure to filter resource response content. The functions
  // of this structure will be called on the browser process IO thread.
  TCefResponseFilter = record
    // Base structure.
    base: TCefBase;

    // Initialize the response filter. Will only be called a single time. The
    // filter will not be installed if this function returns false (0).
    init_filter: function(self: PCefResponseFilter): Integer; stdcall;

    // Called to filter a chunk of data. |data_in| is the input buffer containing
    // |data_in_size| bytes of pre-filter data (|data_in| will be NULL if
    // |data_in_size| is zero). |data_out| is the output buffer that can accept up
    // to |data_out_size| bytes of filtered output data. Set |data_in_read| to the
    // number of bytes that were read from |data_in|. Set |data_out_written| to
    // the number of bytes that were written into |data_out|. If some or all of
    // the pre-filter data was read successfully but more data is needed in order
    // to continue filtering (filtered output is pending) return
    // RESPONSE_FILTER_NEED_MORE_DATA. If some or all of the pre-filter data was
    // read successfully and all available filtered output has been written return
    // RESPONSE_FILTER_DONE. If an error occurs during filtering return
    // RESPONSE_FILTER_ERROR. This function will be called repeatedly until there
    // is no more data to filter (resource response is complete), |data_in_read|
    // matches |data_in_size| (all available pre-filter bytes have been read), and
    // the function returns RESPONSE_FILTER_DONE or RESPONSE_FILTER_ERROR. Do not
    // keep a reference to the buffers passed to this function.
    filter: function(self: PCefResponseFilter; data_in: Pointer; data_in_size, data_in_read: NativeUInt;
        data_out: Pointer; data_out_size, data_out_written: NativeUInt): TCefResponseFilterStatus; stdcall;
  end;

  // Return the alpha byte from a cef_color_t value.
function CefColorGetA(color: TCefColor): Byte; // (((color) >> 24) & 0xFF)
// Return the red byte from a cef_color_t value.
function CefColorGetR(color: TCefColor): Byte; // (((color) >> 16) & 0xFF)
// Return the green byte from a cef_color_t value.
function CefColorGetG(color: TCefColor): Byte; // (((color) >>  8) & 0xFF)
// Return the blue byte from a cef_color_t value.
function CefColorGetB(color: TCefColor): Byte; // (((color) >>  0) & 0xFF)

// Return an cef_color_t value with the specified byte component values.
function CefColorSetARGB(a, r, g, b: Byte): TCefColor;
// static_cast<cef_color_t>( \
// (static_cast<unsigned>(a) << 24) | \
// (static_cast<unsigned>(r) << 16) | \
// (static_cast<unsigned>(g) << 8) | \
// (static_cast<unsigned>(b) << 0))

// Return an int64 value with the specified low and high int32 component values.
function CefInt64Set(int32_low, int32_high: Integer): Int64;
// static_cast<int64>((static_cast<uint32>(int32_low)) | \
// (static_cast<int64>(static_cast<int32>(int32_high))) << 32)

// Return the low int32 value from an int64 value.
function CefInt64GetLow(const int64_val: Int64): Integer;
// static_cast<int32>(int64_val)
// Return the high int32 value from an int64 value.
function CefInt64GetHigh(const int64_val: Int64): Integer;
// static_cast<int32>((static_cast<int64>(int64_val) >> 32) & 0xFFFFFFFFL)

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;
  output: PCefStringWide): Integer;

function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt;
  output: PCefStringUtf8): Integer;

function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt;
  output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

function cef_string_copy(const src: PCefChar; src_len: NativeUInt;
  output: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

var
  // These functions set string values. If |copy| is true (1) the value will be
  // copied instead of referenced. It is up to the user to properly manage
  // the lifespan of references.

  cef_string_wide_set: function(const src: PWideChar; src_len: NativeUInt;
    output: PCefStringWide; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf8_set: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefStringUtf8; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf16_set: function(const src: PChar16; src_len: NativeUInt;
    output: PCefStringUtf16; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_set: function(const src: PCefChar; src_len: NativeUInt;
    output: PCefString; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // These functions clear string values. The structure itself is not freed.

  cef_string_wide_clear: procedure(str: PCefStringWide); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf8_clear: procedure(str: PCefStringUtf8); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf16_clear: procedure(str: PCefStringUtf16); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_clear: procedure(str: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // These functions compare two string values with the same results as strcmp().

  cef_string_wide_cmp: function(const str1, str2: PCefStringWide): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf8_cmp: function(const str1, str2: PCefStringUtf8): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf16_cmp: function(const str1, str2: PCefStringUtf16): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // These functions convert between UTF-8, -16, and -32 strings. They are
  // potentially slow so unnecessary conversions should be avoided. The best
  // possible result will always be written to |output| with the boolean return
  // value indicating whether the conversion is 100% valid.

  cef_string_wide_to_utf8: function(const src: PWideChar; src_len: NativeUInt;
    output: PCefStringUtf8): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf8_to_wide: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_string_wide_to_utf16: function(const src: PWideChar; src_len: NativeUInt;
    output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf16_to_wide: function(const src: PChar16; src_len: NativeUInt;
    output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_string_utf8_to_utf16: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_utf16_to_utf8: function(const src: PChar16; src_len: NativeUInt;
    output: PCefStringUtf8): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_string_to_utf8: function(const src: PCefChar; src_len: NativeUInt;
    output: PCefStringUtf8): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_from_utf8: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_to_utf16: function(const src: PCefChar; src_len: NativeUInt;
    output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_from_utf16: function(const src: PChar16; src_len: NativeUInt;
    output: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_to_wide: function(const src: PCefChar; src_len: NativeUInt;
    output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_from_wide: function(const src: PWideChar; src_len: NativeUInt;
    output: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // These functions convert an ASCII string, typically a hardcoded constant, to a
  // Wide/UTF16 string. Use instead of the UTF8 conversion routines if you know
  // the string is ASCII.

  cef_string_ascii_to_wide: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_ascii_to_utf16: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_from_ascii: function(const src: PAnsiChar; src_len: NativeUInt;
    output: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // These functions allocate a new string structure. They must be freed by
  // calling the associated free function.

  cef_string_userfree_wide_alloc: function(): PCefStringUserFreeWide;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_userfree_utf8_alloc: function(): PCefStringUserFreeUtf8;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_userfree_utf16_alloc: function(): PCefStringUserFreeUtf16;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_userfree_alloc: function(): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // These functions free the string structure allocated by the associated
  // alloc function. Any string contents will first be cleared.

  cef_string_userfree_wide_free: procedure(str: PCefStringUserFreeWide);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_userfree_utf8_free: procedure(str: PCefStringUserFreeUtf8);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_userfree_utf16_free: procedure(str: PCefStringUserFreeUtf16);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_userfree_free: procedure(str: PCefStringUserFree); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

var
  // Create a new browser window using the window parameters specified by
  // |windowInfo|. All values will be copied internally and the actual window will
  // be created on the UI thread. If |request_context| is NULL the global request
  // context will be used. This function can be called on any browser process
  // thread and will not block.
  cef_browser_host_create_browser: function(const windowInfo: PCefWindowInfo;
    client: PCefClient; const url: PCefString;
    const settings: PCefBrowserSettings; request_context: PCefRequestContext)
    : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new browser window using the window parameters specified by
  // |windowInfo|. If |request_context| is NULL the global request context will be
  // used. This function can only be called on the browser process UI thread.
  cef_browser_host_create_browser_sync
    : function(const windowInfo: PCefWindowInfo; client: PCefClient;
    const url: PCefString; const settings: PCefBrowserSettings;
    request_context: PCefRequestContext): PCefBrowser; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Perform a single iteration of CEF message loop processing. This function is
  // used to integrate the CEF message loop into an existing application message
  // loop. Care must be taken to balance performance against excessive CPU usage.
  // This function should only be called on the main application thread and only
  // if cef_initialize() is called with a CefSettings.multi_threaded_message_loop
  // value of false (0). This function will not block.
  cef_do_message_loop_work: procedure(); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Run the CEF message loop. Use this function instead of an application-
  // provided message loop to get the best balance between performance and CPU
  // usage. This function should only be called on the main application thread and
  // only if cef_initialize() is called with a
  // CefSettings.multi_threaded_message_loop value of false (0). This function
  // will block until a quit message is received by the system.
  cef_run_message_loop: procedure; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Quit the CEF message loop that was started by calling cef_run_message_loop().
  // This function should only be called on the main application thread and only
  // if cef_run_message_loop() was used.
  cef_quit_message_loop: procedure; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};


  // Set to true (1) before calling Windows APIs like TrackPopupMenu that enter a

  // modal message loop. Set to false (0) after exiting the modal message loop.
  cef_set_osmodal_loop: procedure(osModalLoop: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Call during process startup to enable High-DPI support on Windows 7 or newer.
  // Older versions of Windows should be left DPI-unaware because they do not
  // support DirectWrite and GDI fonts are kerned very badly.
  cef_enable_highdpi_support: procedure; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // This function should be called from the application entry point function to
  // execute a secondary process. It can be used to run secondary processes from
  // the browser client executable (default behavior) or from a separate
  // executable specified by the CefSettings.browser_subprocess_path value. If
  // called for the browser process (identified by no "type" command-line value)
  // it will return immediately with a value of -1. If called for a recognized
  // secondary process it will block until the process should exit and then return
  // the process exit code. The |application| parameter may be NULL. The
  // |windows_sandbox_info| parameter is only used on Windows and may be NULL (see
  // cef_sandbox_win.h for details).
  cef_execute_process: function(const args: PCefMainArgs; application: PCefApp;
    windows_sandbox_info: Pointer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // This function should be called on the main application thread to initialize
  // the CEF browser process. The |application| parameter may be NULL. A return
  // value of true (1) indicates that it succeeded and false (0) indicates that it
  // failed.
  cef_initialize: function(const args: PCefMainArgs;
    const settings: PCefSettings; application: PCefApp;
    windows_sandbox_info: Pointer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // This function should be called on the main application thread to shut down
  // the CEF browser process before the application exits.
  cef_shutdown: procedure(); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Allocate a new string map.
  cef_string_map_alloc: function(): TCefStringMap; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // function cef_string_map_size(map: TCefStringMap): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  cef_string_map_size: function(map: TCefStringMap): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Return the value assigned to the specified key.
  cef_string_map_find: function(map: TCefStringMap; const key: PCefString;
    var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Return the key at the specified zero-based string map index.
  cef_string_map_key: function(map: TCefStringMap; index: Integer;
    var key: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Return the value at the specified zero-based string map index.
  cef_string_map_value: function(map: TCefStringMap; index: Integer;
    var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Append a new key/value pair at the end of the string map.
  cef_string_map_append: function(map: TCefStringMap;
    const key, value: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Clear the string map.
  cef_string_map_clear: procedure(map: TCefStringMap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Free the string map.
  cef_string_map_free: procedure(map: TCefStringMap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Allocate a new string map.
  cef_string_list_alloc: function(): TCefStringList; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Return the number of elements in the string list.
  cef_string_list_size: function(list: TCefStringList): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Retrieve the value at the specified zero-based string list index. Returns
  // true (1) if the value was successfully retrieved.
  cef_string_list_value: function(list: TCefStringList; index: Integer;
    value: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Append a new value at the end of the string list.
  cef_string_list_append: procedure(list: TCefStringList;
    const value: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Clear the string list.
  cef_string_list_clear: procedure(list: TCefStringList); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Free the string list.
  cef_string_list_free: procedure(list: TCefStringList); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Creates a copy of an existing string list.
  cef_string_list_copy: function(list: TCefStringList): TCefStringList;

  // Register a new V8 extension with the specified JavaScript extension code and
  // handler. Functions implemented by the handler are prototyped using the
  // keyword 'native'. The calling of a native function is restricted to the scope
  // in which the prototype of the native function is defined. This function may
  // only be called on the render process main thread.
  //
  // Example JavaScript extension code:
  //
  // // create the 'example' global object if it doesn't already exist.
  // if (!example)
  // example = {};
  // // create the 'example.test' global object if it doesn't already exist.
  // if (!example.test)
  // example.test = {};
  // (function() {
  // // Define the function 'example.test.myfunction'.
  // example.test.myfunction = function() {
  // // Call CefV8Handler::Execute() with the function name 'MyFunction'
  // // and no arguments.
  // native function MyFunction();
  // return MyFunction();
  // };
  // // Define the getter function for parameter 'example.test.myparam'.
  // example.test.__defineGetter__('myparam', function() {
  // // Call CefV8Handler::Execute() with the function name 'GetMyParam'
  // // and no arguments.
  // native function GetMyParam();
  // return GetMyParam();
  // });
  // // Define the setter function for parameter 'example.test.myparam'.
  // example.test.__defineSetter__('myparam', function(b) {
  // // Call CefV8Handler::Execute() with the function name 'SetMyParam'
  // // and a single argument.
  // native function SetMyParam();
  // if(b) SetMyParam(b);
  // });
  //
  // // Extension definitions can also contain normal JavaScript variables
  // // and functions.
  // var myint = 0;
  // example.test.increment = function() {
  // myint += 1;
  // return myint;
  // };
  // })();
  //
  // Example usage in the page:
  //
  // // Call the function.
  // example.test.myfunction();
  // // Set the parameter.
  // example.test.myparam = value;
  // // Get the parameter.
  // value = example.test.myparam;
  // // Call another function.
  // example.test.increment();
  //
  cef_register_extension: function(const extension_name, javascript_code
    : PCefString; handler: PCefv8Handler): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Register a scheme handler factory with the global request context. An NULL
  // |domain_name| value for a standard scheme will cause the factory to match all
  // domain names. The |domain_name| value will be ignored for non-standard
  // schemes. If |scheme_name| is a built-in scheme and no handler is returned by
  // |factory| then the built-in scheme handler factory will be called. If
  // |scheme_name| is a custom scheme then you must also implement the
  // cef_app_t::on_register_custom_schemes() function in all processes. This
  // function may be called multiple times to change or remove the factory that
  // matches the specified |scheme_name| and optional |domain_name|. Returns false
  // (0) if an error occurs. This function may be called on any thread in the
  // browser process. Using this function is equivalent to calling cef_request_tCo
  // ntext::cef_request_context_get_global_context()->register_scheme_handler_fact
  // ory().
  cef_register_scheme_handler_factory: function(
      const scheme_name, domain_name: PCefString;
      factory: PCefSchemeHandlerFactory): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Clear all scheme handler factories registered with the global request
  // context. Returns false (0) on error. This function may be called on any
  // thread in the browser process. Using this function is equivalent to calling c
  // ef_request_tContext::cef_request_context_get_global_context()->clear_scheme_h
  // andler_factories().
  cef_clear_scheme_handler_factories: function: Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Add an entry to the cross-origin access whitelist.
  //
  // The same-origin policy restricts how scripts hosted from different origins
  // (scheme + domain + port) can communicate. By default, scripts can only access
  // resources with the same origin. Scripts hosted on the HTTP and HTTPS schemes
  // (but no other schemes) can use the "Access-Control-Allow-Origin" header to
  // allow cross-origin requests. For example, https://source.example.com can make
  // XMLHttpRequest requests on http://target.example.com if the
  // http://target.example.com request returns an "Access-Control-Allow-Origin:
  // https://source.example.com" response header.
  //
  // Scripts in separate frames or iframes and hosted from the same protocol and
  // domain suffix can execute cross-origin JavaScript if both pages set the
  // document.domain value to the same domain suffix. For example,
  // scheme://foo.example.com and scheme://bar.example.com can communicate using
  // JavaScript if both domains set document.domain="example.com".
  //
  // This function is used to allow access to origins that would otherwise violate
  // the same-origin policy. Scripts hosted underneath the fully qualified
  // |source_origin| URL (like http://www.example.com) will be allowed access to
  // all resources hosted on the specified |target_protocol| and |target_domain|.
  // If |target_domain| is non-NULL and |allow_target_subdomains| if false (0)
  // only exact domain matches will be allowed. If |target_domain| contains a top-
  // level domain component (like "example.com") and |allow_target_subdomains| is
  // true (1) sub-domain matches will be allowed. If |target_domain| is NULL and
  // |allow_target_subdomains| if true (1) all domains and IP addresses will be
  // allowed.
  //
  // This function cannot be used to bypass the restrictions on local or display
  // isolated schemes. See the comments on CefRegisterCustomScheme for more
  // information.
  //
  // This function may be called on any thread. Returns false (0) if
  // |source_origin| is invalid or the whitelist cannot be accessed.

  cef_add_cross_origin_whitelist_entry: function(const source_origin,
    target_protocol, target_domain: PCefString;
    allow_target_subdomains: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Remove an entry from the cross-origin access whitelist. Returns false (0) if
  // |source_origin| is invalid or the whitelist cannot be accessed.
  cef_remove_cross_origin_whitelist_entry: function(const source_origin,
    target_protocol, target_domain: PCefString;
    allow_target_subdomains: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Remove all entries from the cross-origin access whitelist. Returns false (0)
  // if the whitelist cannot be accessed.
  cef_clear_cross_origin_whitelist: function: Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns true (1) if called on the specified thread. Equivalent to using
  // cef_task_tRunner::GetForThread(threadId)->belongs_to_current_thread().
  cef_currently_on: function(threadId: TCefThreadId): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Post a task for execution on the specified thread. Equivalent to using
  // cef_task_tRunner::GetForThread(threadId)->PostTask(task).
  cef_post_task: function(threadId: TCefThreadId; task: PCefTask): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Post a task for delayed execution on the specified thread. Equivalent to
  // using cef_task_tRunner::GetForThread(threadId)->PostDelayedTask(task,
  // delay_ms).
  cef_post_delayed_task: function(threadId: TCefThreadId; task: PCefTask;
    delay_ms: Int64): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Parse the specified |url| into its component parts. Returns false (0) if the
  // URL is NULL or invalid.
  cef_parse_url: function(const url: PCefString; var parts: TCefUrlParts)
    : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a URL from the specified |parts|, which must contain a non-NULL spec
  // or a non-NULL host and path (at a minimum), but not both. Returns false (0)
  // if |parts| isn't initialized as described.
  cef_create_url: function(parts: PCefUrlParts; url: PCefString): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // This is a convenience function for formatting a URL in a concise and human-
  // friendly way to help users make security-related decisions (or in other
  // circumstances when people need to distinguish sites, origins, or otherwise-
  // simplified URLs from each other). Internationalized domain names (IDN) may be
  // presented in Unicode if |languages| accepts the Unicode representation. The
  // returned value will (a) omit the path for standard schemes, excepting file
  // and filesystem, and (b) omit the port if it is the default for the scheme. Do
  // not use this for URLs which will be parsed or sent to other applications.
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_format_url_for_security_display: function(const origin_url, languages: PCefString): PCefStringUserFree;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the mime type for the specified file extension or an NULL string if
  // unknown.
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_get_mime_type: function(const extension: PCefString): PCefStringUserFree;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Get the extensions associated with the given mime type. This should be passed
  // in lower case. There could be multiple extensions for a given mime type, like
  // "html,htm" for "text/html", or "txt,text,html,..." for "text/*". Any existing
  // elements in the provided vector will not be erased.
  cef_get_extensions_for_mime_type: procedure(const mime_type: PCefString;
    extensions: TCefStringList); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  //******************************************************************************

  // Encodes |data| as a base64 string.
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_base64encode: function(const data: Pointer; data_size: NativeUInt): PCefStringUserFree;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Decodes the base64 encoded string |data|. The returned value will be NULL if
  // the decoding fails.
  cef_base64decode: function(const data: PCefString): PCefBinaryValue;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Escapes characters in |text| which are unsuitable for use as a query
  // parameter value. Everything except alphanumerics and -_.!~*'() will be
  // converted to "%XX". If |use_plus| is true (1) spaces will change to "+". The
  // result is basically the same as encodeURIComponent in Javacript.
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_uriencode: function(const text: PCefString; use_plus: Integer): PCefStringUserFree;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Unescapes |text| and returns the result. Unescaping consists of looking for
  // the exact pattern "%XX" where each X is a hex digit and converting to the
  // character with the numerical value of those digits (e.g. "i%20=%203%3b"
  // unescapes to "i = 3;"). If |convert_to_utf8| is true (1) this function will
  // attempt to interpret the initial decoded result as UTF-8. If the result is
  // convertable into UTF-8 it will be returned as converted. Otherwise the
  // initial decoded result will be returned.  The |unescape_rule| parameter
  // supports further customization the decoding process.
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_uridecode: function(const text: PCefString; convert_to_utf8: Integer;
    unescape_rule: TCefUriUnescapeRule): PCefStringUserFree;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
	
  // Parses |string| which represents a CSS color value. If |strict| is true (1)
  // strict parsing rules will be applied. Returns true (1) on success or false
  // (0) on error. If parsing succeeds |color| will be set to the color value
  // otherwise |color| will remain unchanged.
  cef_parse_csscolor: function(const str: PCefString; strict: Integer;
    color: PCefColor): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

{$ifdef Win32}
  // Parses the specified |json_string| and returns a dictionary or list
  // representation. If JSON parsing fails this function returns NULL.
  cef_parse_json: function(const json_string: PCefString; options: TCefJsonParserOptions): PCefValue;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Parses the specified |json_string| and returns a dictionary or list
  // representation. If JSON parsing fails this function returns NULL and
  // populates |error_code_out| and |error_msg_out| with an error code and a
  // formatted error message respectively.
  cef_parse_jsonand_return_error: function(
      const json_string: PCefString; options: TCefJsonParserOptions;
      error_code_out: PCefJsonParserError; error_msg_out: PCefString): PCefValue;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Generates a JSON string from the specified root |node| which should be a
  // dictionary or list value. Returns an NULL string on failure. This function
  // requires exclusive access to |node| including any underlying data.
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_write_json: function(node: PCefValue; options: TCefJsonWriterOptions): PCefStringUserFree;
    {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
{$endif}

//******************************************************************************

  // Create a new TCefRequest object.
  cef_request_create: function(): PCefRequest; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new TCefPostData object.
  cef_post_data_create: function(): PCefPostData; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_post_data_Element object.
  cef_post_data_element_create: function(): PCefPostDataElement; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_stream_reader_t object from a file.
  cef_stream_reader_create_for_file: function(const fileName: PCefString)
    : PCefStreamReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_stream_reader_t object from data.
  cef_stream_reader_create_for_data: function(data: Pointer; size: NativeUInt)
    : PCefStreamReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_stream_reader_t object from a custom handler.
  cef_stream_reader_create_for_handler: function(handler: PCefReadHandler)
    : PCefStreamReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_stream_writer_t object for a file.
  cef_stream_writer_create_for_file: function(const fileName: PCefString)
    : PCefStreamWriter; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_stream_writer_t object for a custom handler.
  cef_stream_writer_create_for_handler: function(handler: PCefWriteHandler)
    : PCefStreamWriter; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the current (top) context object in the V8 context stack.
  cef_v8context_get_current_context: function(): PCefv8Context; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the entered (bottom) context object in the V8 context stack.
  cef_v8context_get_entered_context: function(): PCefv8Context; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns true (1) if V8 is currently inside a context.
  cef_v8context_in_context: function(): Integer;

  // Create a new cef_v8value_t object of type undefined.
  cef_v8value_create_undefined: function(): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type null.
  cef_v8value_create_null: function(): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type bool.
  cef_v8value_create_bool: function(value: Integer): PCefv8Value;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type int.
  cef_v8value_create_int: function(value: Integer): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type unsigned int.
  cef_v8value_create_uint: function(value: Cardinal): PCefv8Value;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type double.
  cef_v8value_create_double: function(value: Double): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type Date. This function should only be
  // called from within the scope of a cef_render_process_handler_t,
  // cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
  // enter() and exit() on a stored cef_v8context_t reference.
  cef_v8value_create_date: function(const value: PCefTime): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type string.
  cef_v8value_create_string: function(const value: PCefString): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_v8value_t object of type object with optional accessor. This
  // function should only be called from within the scope of a
  // cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
  // or in combination with calling enter() and exit() on a stored cef_v8context_t
  // reference.
  cef_v8value_create_object: function(accessor: PCefV8Accessor): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  // Create a new cef_v8value_t object of type array with the specified |length|.
  // If |length| is negative the returned array will have length 0. This function
  // should only be called from within the scope of a
  // cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
  // or in combination with calling enter() and exit() on a stored cef_v8context_t
  // reference.
  cef_v8value_create_array: function(length: Integer): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_v8value_t object of type function. This function should only
  // be called from within the scope of a cef_render_process_handler_t,
  // cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
  // enter() and exit() on a stored cef_v8context_t reference.
  cef_v8value_create_function: function(const name: PCefString;
    handler: PCefv8Handler): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the stack trace for the currently active context. |frame_limit| is
  // the maximum number of frames that will be captured.
  cef_v8stack_trace_get_current: function(frame_limit: Integer)
    : PCefV8StackTrace; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_xml_reader_t object. The returned object's functions can
  // only be called from the thread that created the object.
  cef_xml_reader_create: function(stream: PCefStreamReader;
    encodingType: TCefXmlEncodingType; const URI: PCefString): PCefXmlReader;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_zip_reader_t object. The returned object's functions can
  // only be called from the thread that created the object.
  cef_zip_reader_create: function(stream: PCefStreamReader): PCefZipReader;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Allocate a new string multimap.
  cef_string_multimap_alloc: function: TCefStringMultimap; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Return the number of elements in the string multimap.
  cef_string_multimap_size: function(map: TCefStringMultimap): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Return the number of values with the specified key.
  cef_string_multimap_find_count: function(map: TCefStringMultimap;
    const key: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Return the value_index-th value with the specified key.
  cef_string_multimap_enumerate: function(map: TCefStringMultimap;
    const key: PCefString; value_index: Integer; var value: TCefString)
    : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Return the key at the specified zero-based string multimap index.
  cef_string_multimap_key: function(map: TCefStringMultimap; index: Integer;
    var key: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Return the value at the specified zero-based string multimap index.
  cef_string_multimap_value: function(map: TCefStringMultimap; index: Integer;
    var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Append a new key/value pair at the end of the string multimap.
  cef_string_multimap_append: function(map: TCefStringMultimap;
    const key, value: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Clear the string multimap.
  cef_string_multimap_clear: procedure(map: TCefStringMultimap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Free the string multimap.
  cef_string_multimap_free: procedure(map: TCefStringMultimap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_build_revision: function: Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the global cookie manager. By default data will be stored at
  // CefSettings.cache_path if specified or in memory otherwise. If |callback| is
  // non-NULL it will be executed asnychronously on the IO thread after the
  // manager's storage has been initialized. Using this function is equivalent to
  // calling cef_request_tContext::cef_request_context_get_global_context()->get_d
  // efault_cookie_manager().
  cef_cookie_manager_get_global_manager: function(
    callback: PCefCompletionCallback): PCefCookieManager; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a new cookie manager. If |path| is NULL data will be stored in memory
  // only. Otherwise, data will be stored at the specified |path|. To persist
  // session cookies (cookies without an expiry date or validity interval) set
  // |persist_session_cookies| to true (1). Session cookies are generally intended
  // to be transient and most Web browsers do not persist them. If |callback| is
  // non-NULL it will be executed asnychronously on the IO thread after the
  // manager's storage has been initialized.
  cef_cookie_manager_create_manager: function(const path: PCefString; persist_session_cookies: Integer;
    callback: PCefCompletionCallback): PCefCookieManager; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};


  // Create a new cef_command_line_t instance.

  cef_command_line_create: function(): PCefCommandLine; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the singleton global cef_command_line_t object. The returned object
  // will be read-only.
  cef_command_line_get_global: function(): PCefCommandLine; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_process_message_t object with the specified name.
  cef_process_message_create: function(const name: PCefString)
    : PCefProcessMessage; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a new object.
  cef_value_create: function(): PCefValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a new object that is not owned by any other object. The specified
  // |data| will be copied.
  cef_binary_value_create: function(const data: Pointer; data_size: NativeUInt)
    : PCefBinaryValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a new object that is not owned by any other object.
  cef_dictionary_value_create: function: PCefDictionaryValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a new object that is not owned by any other object.
  cef_list_value_create: function: PCefListValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Retrieve the path associated with the specified |key|. Returns true (1) on
  // success. Can be called on any thread in the browser process.
  cef_get_path: function(key: TCefPathKey; path: PCefString): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Launches the process specified via |command_line|. Returns true (1) upon
  // success. Must be called on the browser process TID_PROCESS_LAUNCHER thread.
  //
  // Unix-specific notes: - All file descriptors open in the parent process will
  // be closed in the
  // child process except for stdin, stdout, and stderr.
  // - If the first argument on the command line does not contain a slash,
  // PATH will be searched. (See man execvp.)
  cef_launch_process: function(command_line: PCefCommandLine): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new cef_response_t object.
  cef_response_create: function: PCefResponse; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Create a new URL request. Only GET, POST, HEAD, DELETE and PUT request
  // functions are supported. Multiple post data elements are not supported and
  // elements of type PDE_TYPE_FILE are only supported for requests originating
  // from the browser process. Requests originating from the render process will
  // receive the same handling as requests originating from Web content -- if the
  // response contains Content-Disposition or Mime-Type header values that would
  // not normally be rendered then the response may receive special handling
  // inside the browser (for example, via the file download code path instead of
  // the URL request code path). The |request| object will be marked as read-only
  // after calling this function. In the browser process if |request_context| is
  // NULL the global request context will be used. In the render process
  // |request_context| must be NULL and the context associated with the current
  // renderer process' browser will be used.
  cef_urlrequest_create: function(request: PCefRequest; client: PCefUrlRequestClient;
    request_context: PCefRequestContext): PCefUrlRequest; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Visit web plugin information.
  cef_visit_web_plugin_info: procedure(visitor: PCefWebPluginInfoVisitor);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Cause the plugin list to refresh the next time it is accessed regardless of
  // whether it has already been loaded. Can be called on any thread in the
  // browser process.
  cef_refresh_web_plugins: procedure; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
  
  // Add a plugin path (directory + file). This change may not take affect until
  // after cef_refresh_web_plugins() is called. Can be called on any thread in the
  // browser process.
  cef_add_web_plugin_path: procedure(const path: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Add a plugin directory. This change may not take affect until after
  // cef_refresh_web_plugins() is called. Can be called on any thread in the
  // browser process.
  cef_add_web_plugin_directory: procedure(const dir: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Remove a plugin path (directory + file). This change may not take affect
  // until after cef_refresh_web_plugins() is called. Can be called on any thread
  // in the browser process.
  cef_remove_web_plugin_path: procedure(const path: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Unregister an internal plugin. This may be undone the next time
  // cef_refresh_web_plugins() is called. Can be called on any thread in the
  // browser process.
  cef_unregister_internal_web_plugin: procedure(const path: PCefString);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Force a plugin to shutdown. Can be called on any thread in the browser
  // process but will be executed on the IO thread.
  cef_force_web_plugin_shutdown: procedure(const path: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Register a plugin crash. Can be called on any thread in the browser process
  // but will be executed on the IO thread.
  cef_register_web_plugin_crash: procedure(const path: PCefString);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Query if a plugin is unstable. Can be called on any thread in the browser
  // process.
  cef_is_web_plugin_unstable: procedure(const path: PCefString;
    callback: PCefWebPluginUnstableCallback); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Request a one-time geolocation update. This function bypasses any user
  // permission checks so should only be used by code that is allowed to access
  // location information.
  cef_get_geolocation: function(callback: PCefGetGeolocationCallback): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the task runner for the current thread. Only CEF threads will have
  // task runners. An NULL reference will be returned if this function is called
  // on an invalid thread.
  cef_task_runner_get_for_current_thread: function: PCefTaskRunner;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the task runner for the specified CEF thread.
  cef_task_runner_get_for_thread: function(threadId: TCefThreadId)
    : PCefTaskRunner; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};



  // Start tracing events on all processes. Tracing is initialized asynchronously
  // and |callback| will be executed on the UI thread after initialization is
  // complete.
  //
  // If CefBeginTracing was called previously, or if a CefEndTracingAsync call is
  // pending, CefBeginTracing will fail and return false (0).
  //
  // |categories| is a comma-delimited list of category wildcards. A category can
  // have an optional '-' prefix to make it an excluded category. Having both
  // included and excluded categories in the same list is not supported.
  //
  // Example: "test_MyTest*" Example: "test_MyTest*,test_OtherStuff" Example:
  // "-excluded_category1,-excluded_category2"
  //
  // This function must be called on the browser process UI thread.

  cef_begin_tracing: function(const categories: PCefString;
    callback: PCefCompletionCallback): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Stop tracing events on all processes.
  //
  // This function will fail and return false (0) if a previous call to
  // CefEndTracingAsync is already pending or if CefBeginTracing was not called.
  //
  // |tracing_file| is the path at which tracing data will be written and
  // |callback| is the callback that will be executed once all processes have sent
  // their trace data. If |tracing_file| is NULL a new temporary file path will be
  // used. If |callback| is NULL no trace data will be written.
  //
  // This function must be called on the browser process UI thread.

  cef_end_tracing: function(const tracing_file: PCefString;
    callback: PCefEndTracingCallback): Integer;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the current system trace time or, if none is defined, the current
  // high-res time. Can be used by clients to synchronize with the time
  // information in trace events.
  cef_now_from_system_trace_time: function: Int64; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the global context object.
  cef_request_context_get_global_context: function: PCefRequestContext; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Creates a new context object with the specified |settings| and optional
  // |handler|.
  cef_request_context_create_context: function(const settings: PCefRequestContextSettings;
    handler: PCefRequestContextHandler): PCefRequestContext; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};


  // Creates a new context object that shares storage with |other| and uses an
  // optional |handler|.
  create_context_shared: function(other: PCefRequestContext;
    handler: PCefRequestContextHandler): PCefRequestContext;
      {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // See include/base/cef_logging.h for macros and intended usage.

  // Gets the current log level.
  cef_get_min_log_level: function(): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Gets the current vlog level for the given file (usually taken from
  // __FILE__). Note that |N| is the size *with* the null terminator.
  cef_get_vlog_level: function(const file_start: PAnsiChar; n: NativeInt)
    : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Add a log message. See the LogSeverity defines for supported |severity|
  // values.
  cef_log: procedure(const file_: PAnsiChar; line, severity: Integer;
    const message: PAnsiChar); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};




  // Returns the current platform thread ID.

  cef_get_current_platform_thread_id: function(): TCefPlatformThreadId;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the current platform thread handle.
  cef_get_current_platform_thread_handle: function(): TCefPlatformThreadHandle;
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};




  // See include/base/cef_trace_event.h for macros and intended usage.

  // Functions for tracing counters and functions; called from macros.
  // - |category| string must have application lifetime (static or literal). They
  // may not include "(quotes) chars.
  // - |argX_name|, |argX_val|, |valueX_name|, |valeX_val| are optional parameters
  // and represent pairs of name and values of arguments
  // - |copy| is used to avoid memory scoping issues with the |name| and
  // |arg_name| parameters by copying them
  // - |id| is used to disambiguate counters with the same name, or match async
  // trace events

  cef_trace_event_instant: procedure(const Category, name, arg1_name: PAnsiChar;
    arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64;
    copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_event_begin: procedure(const Category, name, arg1_name: PAnsiChar;
    arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64;
    copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_event_end: procedure(const Category, name, arg1_name: PAnsiChar;
    arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64;
    copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_counter: procedure(const Category, name, value1_name: PAnsiChar;
    value1_val: UInt64; const value2_name: PAnsiChar; value2_val: UInt64;
    copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_counter_id: procedure(const Category, name: PAnsiChar; id: UInt64;
    const value1_name: PAnsiChar; value1_val: UInt64;
    const value2_name: PAnsiChar; value2_val: UInt64; copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_event_async_begin: procedure(const Category, name: PAnsiChar;
    id: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64;
    const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_event_async_step_into: procedure(const Category, name: PAnsiChar;
    id, step: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64;
    copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_event_async_step_past: procedure(const Category, name: PAnsiChar;
    id, step: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64;
    copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_trace_event_async_end: procedure(const Category, name: PAnsiChar;
    id: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64;
    const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer);
{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_print_settings_create: function(): PCefPrintSettings;

{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  cef_drag_data_create: function(): PCefDragData;

{$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

  // Returns the global resource bundle instance.

  cef_resource_bundle_get_global: function(): PCefResourceBundle;

  {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};

implementation

function CefColorGetA(color: TCefColor): Byte;
begin
  Result := (color shr 24) and $FF;
end;

function CefColorGetR(color: TCefColor): Byte;
begin
  Result := (color shr 16) and $FF;
end;

function CefColorGetG(color: TCefColor): Byte;
begin
  Result := (color shr 8) and $FF;
end;

function CefColorGetB(color: TCefColor): Byte;
begin
  Result := color and $FF;
end;

function CefColorSetARGB(a, r, g, b: Byte): TCefColor;
begin
  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

function CefInt64Set(int32_low, int32_high: Integer): Int64;
begin
  Result := int32_low or (int32_high shl 32);
end;

function CefInt64GetLow(const int64_val: Int64): Integer;
begin
  Result := Integer(int64_val);
end;

function CefInt64GetHigh(const int64_val: Int64): Integer;
begin
  Result := (int64_val shr 32) and $FFFFFFFF;
end;

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;
  output: PCefStringWide): Integer;
begin
  Result := cef_string_wide_set(src, src_len, output, ord(True))
end;

function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt;
  output: PCefStringUtf8): Integer;
begin
  Result := cef_string_utf8_set(src, src_len, output, ord(True))
end;

function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt;
  output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := cef_string_utf16_set(src, src_len, output, ord(True))
end;

function cef_string_copy(const src: PCefChar; src_len: NativeUInt;
  output: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := cef_string_set(src, src_len, output, ord(True));
end;

end.
