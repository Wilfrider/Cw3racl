#include <cstdlib>
#include <string.h>

#include <stdio.h>
#include <curl/curl.h>

struct MemoryStruct {
  unsigned char *memory;
  size_t size;
};

static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)userp;

  unsigned char *ptr = (unsigned char *)realloc(mem->memory, mem->size + realsize + 1);
  if(!ptr) {
    /* out of memory! */
    // PHP_EXT_LOG("not enough memory (realloc returned NULL)\n");
    return 0;
  }

  mem->memory = ptr;
  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;

  return realsize;
}

extern "C" {
void FreeMallocPoint(void* p){if(p) free(p);}

struct curlWrap_OutStt{
    unsigned char * pHeader;
    int HeaderLen;
    unsigned char * pBody;
    int BodyLen;
};
#define MAX_PATH_ZZ 256
#define SET_CURL_OPT(type, val, errCode) \
    res = curl_easy_setopt(curl, type, val);    if(res != CURLE_OK){ \
        snprintf((char*)pOutStt->pHeader, MAX_PATH_ZZ-1, "failed: %s", curl_easy_strerror(res)); \
        FunRet = errCode; \
        goto exitt; \
    }

int curl_Wrap(const char* reqUrl, const char* proxy, int cnTimeout, int tcpNoDelay, const char* pHeaders, const unsigned char* postFields, int postLen, struct curlWrap_OutStt* pOutStt) {
    CURL *curl;
    CURLcode res;
    int FunRet = 0;

    long response_code;
    struct MemoryStruct retHeader, retBody;

    struct curl_slist *HeadLst = NULL;
    char* curpHd = NULL; const char* LastHdPos; const char* curHdPos; const char* tmpLastPos; const char* tmpLastPos2;
    size_t curHdSize;

    memset(&retHeader, 0, sizeof(retHeader));
    memset(&retBody, 0, sizeof(retBody));
    memset(pOutStt, 0, sizeof(curlWrap_OutStt));
    pOutStt->pHeader = (unsigned char *)malloc(MAX_PATH_ZZ);
    pOutStt->pHeader[MAX_PATH_ZZ-1] = 0;


    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(! curl) {
        snprintf((char*)pOutStt->pHeader, MAX_PATH_ZZ-1, "curl_easy_init failed");
        FunRet = -1;
        goto exitt;
    }

    if (proxy)     curl_easy_setopt(curl, CURLOPT_PROXY, proxy);

    curl_easy_setopt(curl, CURLOPT_URL, reqUrl);


    SET_CURL_OPT(CURLOPT_CONNECTTIMEOUT, cnTimeout, -1);
    SET_CURL_OPT(CURLOPT_TIMEOUT, 5*cnTimeout, -2);

    curl_easy_setopt(curl, CURLOPT_TCP_NODELAY, tcpNoDelay);


    if (postFields && postLen > 0){
        SET_CURL_OPT(CURLOPT_POSTFIELDS, postFields, -3);
        SET_CURL_OPT(CURLOPT_POSTFIELDSIZE, postLen, -4);
        SET_CURL_OPT(CURLOPT_POST, 1L, -41);
    }

    if (pHeaders){
        for (LastHdPos=curHdPos = pHeaders; *curHdPos != 0; curHdPos++){
            if (curHdPos-2 > pHeaders && *curHdPos == '>' && *(curHdPos-2) == '<' && *(curHdPos-1) == '|'){
                curHdSize = curHdPos - LastHdPos + 1 - 2;
                curpHd = (char*)realloc(curpHd, curHdSize); memset(curpHd, 0, curHdSize);
                memmove(curpHd, LastHdPos, curHdSize-1);
                HeadLst = curl_slist_append(HeadLst, curpHd);

                LastHdPos = curHdPos +1;
            }
        }

        SET_CURL_OPT(CURLOPT_HTTPHEADER, HeadLst, -5);
    }


    SET_CURL_OPT( CURLOPT_HEADERFUNCTION, WriteMemoryCallback, -6);
    SET_CURL_OPT( CURLOPT_HEADERDATA, (void *)&retHeader, -7);
    SET_CURL_OPT( CURLOPT_WRITEFUNCTION, WriteMemoryCallback, -8);
    SET_CURL_OPT( CURLOPT_WRITEDATA, (void *)&retBody, -9);


    res = curl_easy_perform(curl); if(res != CURLE_OK){
        snprintf((char*)pOutStt->pHeader, MAX_PATH_ZZ-1, "curl_easy_perform() failed: %s", curl_easy_strerror(res));
        FunRet = -6;
        goto exitt;
    }

    res = curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);     if(res != CURLE_OK){
        snprintf((char*)pOutStt->pHeader, MAX_PATH_ZZ-1, "curl_easy_getinfo failed: %s", curl_easy_strerror(res));
        FunRet = -7;
        goto exitt;
    }

    FunRet = static_cast<int>(response_code);

 exitt:
    if (FunRet > 0){ // OK
        free(pOutStt->pHeader);
        pOutStt->pHeader = retHeader.memory;
        pOutStt->HeaderLen = static_cast<int>(retHeader.size);
        pOutStt->pBody = retBody.memory;
        pOutStt->BodyLen = static_cast<int>(retBody.size);
    }else{
        if (retHeader.memory) free(retHeader.memory);
        if (retBody.memory) free(retBody.memory);
    }

    if (curpHd) free (curpHd);

    if (curl) curl_easy_cleanup(curl);

    if (HeadLst) curl_slist_free_all(HeadLst);
    curl_global_cleanup();

    return FunRet;
}

#include "./secp256k1_recovery.h"

typedef unsigned char uint8_t;
char* ECDSA_sign_web3Wrap(const uint8_t* Len32PrvKey, const uint8_t *Len32Hash, uint8_t *Len65SigOut){
    secp256k1_context* ctx = secp256k1_context_create(SECP256K1_CONTEXT_SIGN);

    secp256k1_nonce_function noncefn = secp256k1_nonce_function_rfc6979;

    secp256k1_ecdsa_recoverable_signature signature;
    memset(&signature, 0, sizeof(signature));

    if (secp256k1_ecdsa_sign_recoverable(ctx, &signature, Len32Hash, Len32PrvKey, noncefn, NULL) == 0)    {
        secp256k1_context_destroy(ctx);
        return "secp256k1_recoverable Fail";
    }

    int recid = 0;
    secp256k1_ecdsa_recoverable_signature_serialize_compact(ctx, Len65SigOut, &recid, &signature);
    secp256k1_context_destroy(ctx);
    if (recid < 256 ) Len65SigOut[64] = static_cast<uint8_t>(recid);
    else {
        return "recidVal over";
    }

    return NULL;
}

} //extern "C"
