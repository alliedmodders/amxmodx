#ifndef INTERFACESYS_H
#define INTERFACESYS_H


#define AMXX_GETINTERFACE(name, addr) MF_RequestInterface(AMXXINTERFACE_##name##_NAME, AMXXINTERFACE_##name##_VERSION, (AMXXInterface **)&addr)


class AMXXInterface
{
public:
	/**
	* @brief Must return an integer defining the interface's version.
	*/
	virtual unsigned int GetInterfaceVersion() = 0;

	/**
	* @brief Must return a string defining the interface's unique name.
	*/
	virtual const char *GetInterfaceName() = 0;

	/**
	* @brief Must return whether the requested version number is backwards compatible.
	* Note: This can be overridden for breaking changes or custom versioning.
	*
	* @param version		Version number to compare against.
	* @return				True if compatible, false otherwise.
	*/
	virtual bool IsVersionCompatible(unsigned int version)
	{
		if (version > GetInterfaceVersion())
		{
			return false;
		}
		
		return true;
	}
};

#endif