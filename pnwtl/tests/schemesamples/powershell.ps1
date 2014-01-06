# ---------------------------------------------------------------------------------------------------
# 
# Microsoft PowerShell Source File
# NAME:		New-group.ps1
# 
# AUTHOR:	Adam Bell, www.leadfollowmove.com
# DATE	:	12/03/2007
# 
# PURPOSE:	Create a new group in Active Directory
#
# COMMENT:	Best viewed in Notepad2
#			Sample code only. Educational purposes only.
# 
# ---------------------------------------------------------------------------------------------------

# Constants from: http://msdn2.microsoft.com/en-us/library/aa772263.aspx
Set-Variable -Name ADS_GROUP_TYPE_DOMAIN_LOCAL_GROUP	-value 4			-option constant
Set-Variable -Name ADS_GROUP_TYPE_GLOBAL_GROUP			-value 2 			-option constant
Set-Variable -Name ADS_GROUP_TYPE_LOCAL_GROUP			-value 4			-option constant
Set-Variable -Name ADS_GROUP_TYPE_UNIVERSAL_GROUP		-value 8 			-option constant
Set-Variable -Name ADS_GROUP_TYPE_SECURITY_ENABLED		-value -2147483648	-option constant

# Global group appears to be the default in PS whhen created so can be ommitted from our constants
Set-Variable -Name ADS_GROUP_TYPE_SECURITY_DOMAIN_LOCAL	`
	-value ($ADS_GROUP_TYPE_DOMAIN_LOCAL_GROUP	`
	-bor $ADS_GROUP_TYPE_SECURITY_ENABLED)	-option constant
	
Set-Variable -Name ADS_GROUP_TYPE_SECURITY_DOMAIN_GLOBAL `
	-value ($ADS_GROUP_TYPE_GLOBAL_GROUP	`
	-bor $ADS_GROUP_TYPE_SECURITY_ENABLED)	-option constant
	
Set-Variable -Name ADS_GROUP_TYPE_SECURITY_UNIVERSAL	`
	-value ($ADS_GROUP_TYPE_UNIVERSAL_GROUP		`
	-bor $ADS_GROUP_TYPE_SECURITY_ENABLED)	-option constant

# ---------------------------------------------------------------------------------------------------
	
	$root	= [adsi]""
	$rootdn	= $root.distinguishedname

# ---------------------------------------------------------------------------------------------------
function create-group
# ---------------------------------------------------------------------------------------------------
{
#	Inputs: 	1) OU / Container where the group os to be created.
#				2) Group Name
#				3) Scope as detailed in the switch block below.
#				4) Description of the group.
#	Objective:	1) Create an AD group
#	Returns:	1) nada.
Param (
	$Location,
	$Group,
	$scope,
	$Description
	
	)
	# The domain DN is added so the OU location doesn't need to be a full DN :)
	# This also doesn't tie you down to a specific Domain.
	$ou = [adsi]("LDAP://"+$Location+","+$rootDN)
	$newGroup = $ou.create("group", "cn="+$Group)
	$newgroup.put("sAmAccountName", $Group)
	$newGroup.Put("Description", $Description)
	
	switch ($scope)
	{
		"Security Domain Local"			{$Type = $ADS_GROUP_TYPE_SECURITY_DOMAIN_LOCAL}
		"Security Domain Global"		{$Type = $ADS_GROUP_TYPE_SECURITY_DOMAIN_GLOBAL}
		"Security Universal"			{$Type = $ADS_GROUP_TYPE_SECURITY_UNIVERSAL}
		"Distribution Domain Local"		{$Type = $ADS_GROUP_TYPE_DOMAIN_LOCAL_GROUP}
		"Distribution Domain Global"	{$Type = $ADS_GROUP_TYPE_GLOBAL_GROUP}
		"Distribution Universal"		{$Type = $ADS_GROUP_TYPE_UNIVERSAL_GROUP}
	}
	$NewGroup.put("grouptype", $Type)
	$newGroup.SetInfo()
}

# ---------------------------------------------------------------------------------------------------

#Sample calling statement:
Create-Group "ou=Test OU" "Test Group" "Security Domain Local" "My Test Group"