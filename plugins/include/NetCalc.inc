// NetCalc for AMX Mod X.
// Copyright Amezoure, 2017. All rights reserved.

#if defined _NetCalc_included
    #endinput
#endif
#define _NetCalc_included

/**
 * The maximum buffer size required to store an IP address.
 */
#define MAX_IP_LENGTH 16

/**
 * Converts IP address to integer.
 */
stock inet_aton(const szIP[]) {
    new szTemp[MAX_IP_LENGTH], szOctet[4], aOctet[4];
    strtok(szIP, szOctet, charsmax(szOctet), szTemp, charsmax(szTemp), '.');

    for (new i = 0; i < 4; i++) {
        aOctet[i] = clamp(str_to_num(szOctet), 0, 255);
        strtok(szTemp, szOctet, charsmax(szOctet), szTemp, charsmax(szTemp), '.');
    }

    return (aOctet[0] << 24) | (aOctet[1] << 16) | (aOctet[2] << 8) | aOctet[3];
}

/**
 * Converts integer to IP address.
 */
stock inet_ntoa(iIP) {
    new szIP[MAX_IP_LENGTH];
    formatex(szIP, charsmax(szIP), "%i.%i.%i.%i", iIP >> 24 & 255, iIP >> 16 & 255, iIP >> 8 & 255, iIP & 255);
    return szIP;
}

/**
 * Converts CIDR notation to integer.
 */
stock inet_mton(const szCIDR[]) {
    new iCIDR = clamp(str_to_num(szCIDR), 0, 32);
    return iCIDR ? 0xFFFFFFFF << (32 - iCIDR) : 0;
}

/**
 * Converts integer to CIDR notation.
 */
stock inet_ntom(iCIDR) {
    new iBits, szCIDR[3];
    for (iBits = 0; iCIDR; iBits++) iCIDR <<= 1;
    num_to_str(iBits, szCIDR, charsmax(szCIDR));
    return szCIDR;
}

/**
 * Gets IP address and CIDR notation as integer.
 */
stock inet_range(const szSubnet[], &iIP, &iCIDR) {
    new szIP[MAX_IP_LENGTH], szCIDR[3];
    strtok(szSubnet, szIP, charsmax(szIP), szCIDR, charsmax(szCIDR), '/');
    iIP = inet_aton(szIP);
    iCIDR = szCIDR[0] ? inet_mton(szCIDR) : 0xFFFFFFFF;
}
